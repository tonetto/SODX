-module(client).
-export([start/9]).

-ifdef(debug_client).
-define(DBG(X,Y,Z), io:format("[CLIENT_DEBUG] ~w: ~s ~w~n", [X, Y, Z])).
-else.
-define(DBG(X,Y,Z), true).
-endif.

start(Name, Entries, Updates, Server, Round, Sleep, Read, Write, Slice) ->
    spawn(fun() ->
                  init(Name, Entries, Updates, Server, 0, 0, Round, Sleep,
                       Read, Write, Slice)
          end).

init(Name, Entries, Updates, Server, Total, Ok, Round, Sleep,
     Read, Write, Slice) ->
    ?DBG(Name,"Initilizing Client",self()),
    open(Name, Entries, Updates, Server, Total, Ok, Round, Sleep,
         Read, Write, Slice).

open(Name, Entries, Updates, Server, Total, Ok, Round, Sleep,
     Read, Write, Slice) ->
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
            MinEntry = random:uniform(Entries),
            SliceSize = round(Entries * Slice),
            if
                (MinEntry + SliceSize) > Entries ->
                    Entries2 = (Entries - SliceSize),
                    do_transactions(Name, Entries, Entries2, Updates, Server,
                                    Handler, Total, Ok, Updates, Round, Sleep,
                                    Read, Read, Write, Write, Slice);
                true ->
                    Entries2 = MinEntry,
                    do_transactions(Name, Entries, Entries2, Updates, Server,
                                    Handler, Total, Ok, Updates, Round, Sleep,
                                    Read, Read, Write, Write, Slice)
            end
    end.

% Commit transaction
do_transactions(Name, Entries, _, Updates, Server, Handler, Total, Ok, 0,
                Round, Sleep, Read, _, Write, _, Slice) ->
    %%%
    %% This line is going to be used to modify the amount of time each
    %% transaction will take
    %%%
    timer:sleep(Sleep),
    Ref = make_ref(),
    Handler ! {commit, Ref},
    Result = receiveCommitValue(Ref),
    if
        Result == ok ->
            open(Name, Entries, Updates, Server, Total+1, Ok+1, Round, Sleep,
                 Read, Write, Slice);
        Result == stop ->
            close(Name, Total, Ok, Round);
        true ->
            open(Name, Entries, Updates, Server, Total+1, Ok, Round, Sleep,
                 Read, Write, Slice)
    end;

% Reads and Writes
do_transactions(Name, Entries, MyEntries, Updates, Server, Handler, Total, Ok,
                N, Round, Sleep, Read, 0, Write, 0, Slice) ->
    do_transactions(Name, Entries, MyEntries, Updates, Server, Handler, Total,
                    Ok, N-1, Round, Sleep, Read, Read, Write, Write, Slice);

do_transactions(Name, Entries, MyEntries, Updates, Server, Handler, Total, Ok,
                N, Round, Sleep, Read, R, Write, 0, Slice) ->
    Ref = make_ref(),
    RndNum = random:uniform(round(Entries*Slice)),
    Num = MyEntries + RndNum,
    Handler ! {read, Ref, Num},
    Value = receiveValue(Ref),
    if
        Value == stop ->
            close(Name, Total, Ok, Round);
        true ->
            do_transactions(Name, Entries, MyEntries, Updates, Server, Handler,
                         Total, Ok, N, Round, Sleep, Read, R-1, Write, 0, Slice)
    end;

do_transactions(Name, Entries, MyEntries, Updates, Server, Handler, Total, Ok,
                N, Round, Sleep, Read, 0, Write, W, Slice) ->
    RndNum = random:uniform(round(Entries*Slice)),
    Num = MyEntries + RndNum,
    Handler ! {write, Num, Name+Num+1},
    do_transactions(Name, Entries, MyEntries, Updates, Server, Handler, Total,
                    Ok, N, Round, Sleep, Read, 0, Write, W-1, Slice);

do_transactions(Name, Entries, MyEntries, Updates, Server, Handler, Total, Ok,
                N, Round, Sleep, Read, R, Write, W, Slice) ->
    Ref = make_ref(),
    RndNum = random:uniform(round(Entries*Slice)),
    Num = MyEntries + RndNum,
    ?DBG(Name,"{RndNum,Num}",{RndNum,Num}),
    Handler ! {read, Ref, Num},
    Value = receiveValue(Ref),
    if
        Value == stop ->
            close(Name, Total, Ok, Round);
        true ->
            Handler ! {write, Num, Value+1},
            do_transactions(Name, Entries, MyEntries, Updates, Server, Handler,
                       Total, Ok, N, Round, Sleep, Read, R-1, Write, W-1, Slice)
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
