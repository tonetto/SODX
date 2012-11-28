-module(entry).
-export([new/1]).

-ifdef(debug).
-define(DBG(X,Y,Z), io:format("[ENTRY_DEBUG] ~w: ~s ~w~n", [X, Y, Z])).
-else.
-define(DBG(X,Y,Z), true).
-endif.

new(Value) ->
    spawn_link(fun() -> init(Value) end).
init(Value) ->
    entry(Value, make_ref()).

entry(Value, Time) ->
    receive
        {read, Ref, From} ->
            ?DBG(self(),"Received a read with Ref",Ref),
            From ! {Ref, self(), Value, Time},
            entry(Value, Time);
        {write, New} ->
            ?DBG(self(),"Received a write with Value",New),
            entry(New, make_ref());
        {check, Ref, Time, From} -> %% use pattern matching is more natural
            ?DBG(self(),"[OK] Received a check for {Ref,Time}",{Ref,Time}),
            From ! {Ref, ok},
            entry(Value, Time);
        {check, Ref, _Readtime, From} -> %% Readtime is not used, we discard it
            ?DBG(self(),"[Abort] Received a check for {Ref,Time}",{Ref,Time}),
            From ! {Ref, abort},
            entry(Value, Time);
        stop ->
            ?DBG(self(),"Stop",[]),
            ok
    end.
