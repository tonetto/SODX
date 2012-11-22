-module(client).
-export([start/4]).

start(Name, Entries, Updates, Server) ->
    spawn(fun() -> open(Name, Entries, Updates, Server, 0, 0) end).

open(Name, Entries, Updates, Server, Total, Ok) ->
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    Server ! {open, self()},
    receive
        {stop, From} ->
            io:format("~w: Transactions TOTAL:~w, OK:~w, -> ~w % ~n",
                      [Name, Total, Ok, 100*Ok/Total]),
            From ! {done, self()},
            ok;
        {transaction, Validator, Store} ->
            Handler = handler:start(self(), Validator, Store),
            do_transactions(Name, Entries, Updates, Server, Handler,
                            Total, Ok, Updates)
    end.

% Commit transaction
do_transactions(Name, Entries, Updates, Server, Handler, Total, Ok, 0) ->
    %io:format("~w: Commit: TOTAL ~w, OK ~w~n", [Name, Total, Ok]),
    %timer:sleep(Name*10),
    Ref = make_ref(),
    Handler ! {commit, Ref},
    Result = receiveCommitValue(Ref),
    if
        Result == ok ->
            open(Name, Entries, Updates, Server, Total+1, Ok+1);
        true ->
            open(Name, Entries, Updates, Server, Total+1, Ok)
    end;

% Reads and Writes
do_transactions(Name, Entries, Updates, Server, Handler, Total, Ok, N) ->
    %io:format("~w: R/W: TOTAL ~w, OK ~w, N ~w~n", [Name, Total, Ok, N]),
    Ref = make_ref(),
    Num = random:uniform(Entries),
    Handler ! {read, Ref, Num},
    Value = receiveValue(Ref),
    Handler ! {write, Num, Value+1},
    do_transactions(Name, Entries, Updates, Server, Handler, Total, Ok, N-1).

receiveCommitValue(Ref) ->
    receive
        {Ref,Value} -> Value
    end.

receiveValue(Ref) ->
    receive
        {value,Ref,Value} -> Value
    end.
