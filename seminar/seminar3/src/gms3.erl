-module(gms3).
-export([start/1, start/2]).

-define(arghh, 100).

-ifdef(debug).
-define(DBG(X), io:format("[DEBUG] ~w: ~s~n", [self(), X])).
-else.
-define(DBG(X), true).
-endif.

start(Id) ->
    Rnd = random:uniform(100),
    Self = self(),
    spawn_link(fun()-> init(Id, Rnd, Self) end).

init(Id, Rnd, Master) ->
    ?DBG("I'm the leader!"),
    random:seed(Rnd, Rnd, Rnd),
    leader(Id, Master, 0, []).

start(Id, Grp) ->
    Rnd = random:uniform(100),
    Self = self(),
    spawn_link(fun()-> init(Id, Grp, Rnd, Self) end).

init(Id, Grp, Rnd, Master) ->
    ?DBG("I'm a Slave!"),
    random:seed(Rnd, Rnd, Rnd),
    Self = self(),
    Grp ! {join, Self},
    receive
        {view, N, State, Leader, Peers} ->
            Last = {view, N, State, Leader, Peers},
            erlang:monitor(process, Leader),
            Master ! {ok, State},
            slave(Id, Master, Leader, N, Last, Peers)
    after 1000 ->
            ?DBG("timeout waiting for the Leader!"),
            Master ! {error, "no reply from leader"}
    end.

slave(Id, Master, Leader, N, Last, Peers) ->
    receive
        {mcast, Msg} ->
            Leader ! {mcast, Msg},
            slave(Id, Master, Leader, N, Last, Peers);
        {join, Peer} ->
            Leader ! {join, Peer},
            slave(Id, Master, Leader, N, Last, Peers);
        {msg, I, _Msg} when I =< N ->
            ?DBG("Receiving a message that I already had!"),
            slave(Id, Master, Leader, N, Last, Peers);
        {msg, I, Msg} when I > N->
            Last2 = {msg, I, Msg},
            Master ! {deliver, Msg},
            slave(Id, Master, Leader, I, Last2, Peers);
        {view, I, _, _, View} ->
            Last2 = {view, I, [], [], View},
            slave(Id, Master, Leader, I, Last2, View);
        {'DOWN', _Ref, process, Leader, _Reason} ->
            ?DBG("The leader is dead!"),
            io:format("[DEBUG] ~w: My last N was ~w~n", [self(), N]),
            election(Id, Master, N, Last, Peers);
        stop ->
            ok;
        Error ->
            io:format("gms ~w: slave, strange message ~w~n", [Id, Error])
    end.

leader(Id, Master, N, Peers) ->
    receive
        {mcast, Msg} ->
            bcast(Id, {msg, N+1, Msg}, Peers),
            Master ! {deliver, Msg},
            leader(Id, Master, N+1, Peers);
        {join, Peer} ->
            Master ! request,
            joining(Id, Master, Peer, N, Peers);
        stop ->
            ok;
        Error ->
            io:format("gms ~w: leader, strange message ~w~n", [Id, Error])
    end.

joining(Id, Master, Peer, N, Peers) ->
    receive
        {ok, State} ->
            Peers2 = lists:append(Peers, [Peer]),
            bcast(Id, {view, N+1, State, self(), Peers2}, Peers2),
            leader(Id, Master, N+1, Peers2);
        stop ->
            ok
    end.

bcast(Id, Msg, Nodes) ->
    lists:foreach(fun(Node) ->
                          Node ! Msg,
                          crash(Id)
                  end,
                  Nodes).

crash(Id) ->
    case random:uniform(?arghh) of
        ?arghh ->
            io:format("leader ~w: crash~n", [Id]),
            exit(no_luck);
        _ ->
            ok
    end.


election(Id, Master, N, Last, [Leader|Rest]) ->
    if
        Leader == self() ->
            ?DBG("I'm the new Leader!"),
            bcast(Id, Last, Rest),
            leader(Id, Master, N, Rest);
        true ->
            ?DBG("New Leader elected!"),
            erlang:monitor(process, Leader),
            slave(Id, Master, Leader, N, Last, Rest)
    end.
