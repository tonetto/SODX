-module(chordy).
-export([connect/1, test/2]).

% ’Peer’ stands for the node used to join the system
connect(Peer) ->
    spawn(fun() -> wait(Peer) end).

wait(Peer) ->
    receive
        {add, Key, Value} ->
            Ref = make_ref(),
            Peer ! {add, Key, Value, Ref, self()},
            receive
                {Ref, ok} ->
                    io:format("[Add] Key added correctly~n")
            end,
            wait(Peer);
        {lookup, Key} ->
            Ref = make_ref(),
            Peer ! {lookup, Key, Ref, self()},
            receive
                {Ref, Value} ->
                    io:format("[Lookup] Key: ~w Value: ~w~n",[Key, Value])
            end,
            wait(Peer);
        {test, NumRequests} ->
            test(Peer, NumRequests),
            wait(Peer)
    end.

test(Peer, NumRequests) ->
    Begin = erlang:now(),
    {KeysAdd, RefsAdd} = add(Peer, NumRequests, [], []),
    io:format("Requests done. Waiting for answers...~n"),
    waitAdds(RefsAdd),
    io:format("Answers received. Making lookups...~n"),
    RefsLook = lookup(Peer, KeysAdd, []),
    io:format("Lookups received. Waiting for values...~n"),
    Values = waitLookups(RefsLook, []),
    End = erlang:now(),
    Elapsed = timer:now_diff(End, Begin)/1000.0,
    io:format("Values received. Checking correctness...~n"),
    checkLookups(Values, KeysAdd),
    io:format("Elapsed Time: ~w ms~n", [Elapsed]).

add(_, 0, KeysDone, RefsDone) ->
    {KeysDone, RefsDone};
add(Peer, NumRequests, KeysDone, RefsDone) ->
    Key = key:generate(),
    Qref = make_ref(),
    Peer ! {add, Key, Key, Qref, self()},
    add(Peer, NumRequests-1, [{Key}|KeysDone], [Qref|RefsDone]).

waitAdds([]) ->
    ok;
waitAdds([Qref|Refs]) ->
    receive
        {Qref, ok} ->
            waitAdds(Refs)
    end.

lookup(_, [], RefsLook) ->
    RefsLook;
lookup(Peer, [{Key}|KeysAdd], RefsLook) ->
    Qref = make_ref(),
    Peer ! {lookup, Key, Qref, self()},
    lookup(Peer, KeysAdd, [Qref|RefsLook]).

waitLookups([], Values) ->
    Values;
waitLookups([Qref|RefsLook], Values) ->
    receive
        {Qref, Value} ->
            waitLookups(RefsLook, [{Value}|Values])
    end.

checkLookups([], _) ->
    io:format("Everything correct!~n"),
    ok;
checkLookups([{Value}|Values], KeysAdd) ->
    case lists:keymember(Value, 1, KeysAdd) of
        true ->
            KeysDeleted = lists:keydelete(Value, 1, KeysAdd),
            checkLookups(Values, KeysDeleted);
        false ->
            io:format("Something’s wrong!~nValues: ~w~nKeys: ~w~n",
                      [[{Value}|Values],KeysAdd]),
            false
    end.
