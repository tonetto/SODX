-module(gms1).
-export([start/1, start/2]).

start(Id) ->
    Self = self(),
    spawn_link(fun()-> init(Id, Self) end).

init(Id, Master) ->
    leader(Id, Master, []).

start(Id, Grp) ->
    Self = self(),
    spawn_link(fun()-> init(Id, Grp, Self) end).

init(Id, Grp, Master) ->
    Self = self(),
    Grp ! {join, Self},
    receive
        {view, State, Leader, Peers} ->
            Master ! {ok, State},
            slave(Id, Master, Leader, Peers)
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
