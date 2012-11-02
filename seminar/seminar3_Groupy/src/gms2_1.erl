-module(gms2_1).
-export([start/1, start/2]).

-ifdef(debug).
-define(DBG(X), io:format("[DEBUG] ~w: ~s~n", [self(), X])).
-else.
-define(DBG(X), true).
-endif.

start(Id) ->
    Self = self(),
    spawn_link(fun()-> init(Id, Self) end).

init(Id, Master) ->
    ?DBG("I'm the leader!"),
    leader(Id, Master, []).

start(Id, Grp) ->
    Self = self(),
    spawn_link(fun()-> init(Id, Grp, Self) end).

init(Id, Grp, Master) ->
    ?DBG("I'm a Slave!"),
    Self = self(),
    Grp ! {join, Self},
    receive
        {view, State, Leader, Peers} ->
            erlang:monitor(process, Leader),
            Master ! {ok, State},
            slave(Id, Master, Leader, Peers)
    after 1000 ->
            Master ! {error, "no reply from leader"}
    end.

slave(Id, Master, Leader, Peers) ->
    receive
        {mcast, Msg} ->
            Leader ! {mcast, Msg},
            slave(Id, Master, Leader,Peers);
        {join, Peer} ->
            Leader ! {join, Peer},
            slave(Id, Master, Leader,Peers);
        {msg, Msg} ->
            Master ! {deliver, Msg},
            slave(Id, Master, Leader,Peers);
        {view, _, _, View} ->
            slave(Id, Master, Leader,View);
        {'DOWN', _Ref, process, Leader, _Reason} ->
            ?DBG("The leader is dead!"),
            election(Id, Master, Peers);
        stop ->
            ok;
        Error ->
            io:format("gms ~w: slave, strange message ~w~n", [Id, Error])
    end.

leader(Id, Master, Peers) ->
    receive
        {mcast, Msg} ->
            bcast(Id, {msg, Msg}, Peers),
            Master ! {deliver, Msg},
            leader(Id, Master, Peers);
        {join, Peer} ->
            Master ! request,
            joining(Id, Master, Peer, Peers);
        stop ->
            ok;
        Error ->
            io:format("gms ~w: leader, strange message ~w~n", [Id, Error])
    end.

joining(Id, Master, Peer, Peers) ->
    receive
        {ok, State} ->
            Peers2 = lists:append(Peers, [Peer]),
            bcast(Id, {view, State, self(), Peers2}, Peers2),
            leader(Id, Master, Peers2);
        stop ->
            ok
    end.

bcast(_, Msg, Nodes) ->
    lists:foreach(fun(Node) -> Node ! Msg end, Nodes).

election(Id, Master, [Leader|Rest]) ->
    if
        Leader == self() ->
            ?DBG("I'm the new Leader!"),
            leader(Id, Master, Rest);
        true ->
            ?DBG("New Leader elected!"),
            erlang:monitor(process, Leader),
            slave(Id, Master, Leader, Rest)
    end.
