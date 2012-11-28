-module(client).
-export([start/5]).

-ifdef(debug_client).
-define(DBG(X,Y,Z), io:format("[CLIENT_DEBUG] ~w: ~s ~w~n", [X, Y, Z])).
-else.
-define(DBG(X,Y,Z), true).
-endif.

start(Name, Entries, Updates, Server, Round) ->
    spawn(fun() -> init(Name, Entries, Updates, Server, 0, 0, Round) end).

init(Name, Entries, Updates, Server, Total, Ok, Round) ->
    ?DBG(Name,"Initilizing Client",self()),
    open(Name, Entries, Updates, Server, Total, Ok, Round).

open(Name, Entries, Updates, Server, Total, Ok, Round) ->
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    Server ! {open, self()},
    receive
        {stop, From} ->
            %io:format("~w: Transactions TOTAL:~w, OK:~w, -> ~w % ~n",
            %          [Name, Total, Ok, 100*Ok/Total]),
            stat ! {statistic, Round, Total, Ok},
            From ! {done, self()},
            ok;
        {transaction, Validator, Store} ->
            ?DBG(Name,"Received a transaction message from the server!",ok),
            Handler = handler:start(self(), Validator, Store),
            do_transactions(Name, Entries, Updates, Server, Handler,
                            Total, Ok, Updates, Round)
    end.

% Commit transaction
do_transactions(Name, Entries, Updates, Server, Handler, Total, Ok, 0, Round) ->
    %%%
    %% This line is going to be used to modify the amount of time each
    %% transaction will take
    %%%
    timer:sleep(SleepTime),
    Ref = make_ref(),
    Handler ! {commit, Ref},
    Result = receiveCommitValue(Ref),
    if
        Result == ok ->
            open(Name, Entries, Updates, Server, Total+1, Ok+1, Round);
        Result == stop ->
            close(Name, Total, Ok, Round);
        true ->
            open(Name, Entries, Updates, Server, Total+1, Ok, Round)
    end;

% Reads and Writes
do_transactions(Name, Entries, Updates, Server, Handler, Total, Ok, N, Round) ->
    Ref = make_ref(),
    Num = random:uniform(Entries),
    Handler ! {read, Ref, Num},
    Value = receiveValue(Ref),
    if
        Value == stop ->
            close(Name, Total, Ok, Round);
        true ->
            Handler ! {write, Num, Value+1},
            do_transactions(Name, Entries, Updates, Server, Handler, Total, Ok, N-1, Round)
    end.

receiveCommitValue(Ref) ->
    ?DBG(self(),"Waiting to receive commits",waiting),
    receive
        {Ref,Value} ->
            ?DBG(self(),"Got the commit",ok),
            Value;
        {stop, From} ->
            ?DBG(self(),"Got a stop from Opty",stop),
            From ! {done, self()},
            stop
    end.

receiveValue(Ref) ->
    ?DBG(self(),"Waiting to receive the Value",waiting),
    receive
        {value,Ref,Value} ->
            ?DBG(self(),"Got the value",ok),
            Value;
        {stop, From} ->
            ?DBG(self(),"Got a stop from Opty",stop),
            From ! {done, self()},
            stop
    end.

close(Name, Total, Ok, Round) ->
    ?DBG(Name,"Stopping and sending statistics in Round", Round),
    stat ! {statistic, Round, Total, Ok},
    ok.
