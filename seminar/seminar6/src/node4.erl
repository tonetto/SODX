-module(node4).
-export([start/1,start/2]).

-ifdef(debug).
-define(DBG(X,Y,Z), io:format("[NODE_DEBUG] ~w: ~s ~w~n", [X, Y, Z])).
-else.
-define(DBG(X,Y,Z), true).
-endif.

-define(Stabilize, 1000).
-define(Timeout, 5000).

start(MyKey) ->
    ?DBG(MyKey,"Starting as the first in the ring",start),
    start(MyKey, nil).

start(MyKey, PeerPid) ->
    ?DBG(MyKey,"Starting with an existing node:",PeerPid),
    timer:start(),
    spawn(fun() -> init(MyKey, PeerPid) end).

init(MyKey, PeerPid) ->
    Predecessor = nil,
    case connect(MyKey, PeerPid) of
        {ok, Successor} ->
            schedule_stabilize(),
            node(MyKey, Predecessor, Successor, [], nil, []);
        _ ->
            ?DBG(MyKey,"Something wrong on the connect function!",init_fail)
    end.

connect(MyKey, nil) ->
    {ok, {MyKey , self()}}; %% TODO

connect(_, PeerPid) ->
    Qref = make_ref(),
    PeerPid ! {key, Qref, self()},
    receive
        {Qref, Skey} ->
            Ref = monit(PeerPid),
            {ok, {Skey, Ref, PeerPid}} %% TODO
    after ?Timeout ->
            io:format("Timeout: no response from ~w~n", [PeerPid])
    end.

schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

node(MyKey, Predecessor, Successor, Store, Next, Replica) ->
    receive
        {key, Qref, PeerPid} ->
            PeerPid ! {Qref, MyKey},
            node(MyKey, Predecessor, Successor, Store, Next, Replica);
        {notify, New} ->
            {Pred, NewStore} = notify(New, MyKey, Predecessor, Store),
            node(MyKey, Pred, Successor, NewStore, Next, Replica);
        {request, Peer} ->
            request(Peer, Predecessor, Successor),
            node(MyKey, Predecessor, Successor, Store, Next, Replica);
        {status, Pred, Nx} ->
            {Succ, Nxt} = stabilize(Pred, MyKey, Successor, Nx),
            node(MyKey, Predecessor, Succ, Store, Nxt, Replica);
        stabilize ->
            ?DBG(MyKey,"Starting a new Stabilize",stabilize),
            stabilize(Successor),
            node(MyKey, Predecessor, Successor, Store, Next, Replica);
        probe ->
            ?DBG(MyKey,"Starting a probe.",probe),
            create_probe(MyKey, Successor),
            node(MyKey, Predecessor, Successor, Store, Next, Replica);
        {probe, MyKey, Nodes, T} ->
            ?DBG(MyKey,"Got my probe back. Ring should be ok.", probe),
            remove_probe(MyKey, Nodes, T),
            node(MyKey, Predecessor, Successor, Store, Next, Replica);
        {probe, RefKey, Nodes, T} ->
            ?DBG(MyKey,"Got another probe, forwarding.",probe),
            forward_probe(RefKey, [MyKey|Nodes], T, Successor),
            node(MyKey, Predecessor, Successor, Store, Next, Replica);
        {add, Key, Value, Qref, Client} ->
            Added = add(Key, Value, Qref, Client,
                        MyKey, Predecessor, Successor, Store),
            node(MyKey, Predecessor, Successor, Added, Next, Replica);
        {lookup, Key, Qref, Client} ->
            lookup(Key, Qref, Client, MyKey, Predecessor, Successor, Store),
            node(MyKey, Predecessor, Successor, Store, Next, Replica);
        {handover, ElementsStore, ElementsReplica} ->
            MergedStore = storage:merge(Store, ElementsStore), %% TODO
            MergedRepl = storage:merge(Replica, ElementsReplica), %% TODO
            node(MyKey, Predecessor, Successor, MergedStore, Next, MergedRepl);
        {replicate, Key, Value} ->
            NewReplica = storage:add(Key, Value, Replica),
            node(MyKey, Predecessor, Successor, Store, Next, NewReplica);
        {'DOWN', Ref, process, _, _} ->
            {Pred, Succ, Nxt, NewStore, NewReplica} = down(Ref, Predecessor,
                                                           Successor, Next,
                                                           Store, Replica),
            node(MyKey, Pred, Succ, NewStore, Nxt, NewReplica);
        _ ->
            node(MyKey, Predecessor, Successor, Store, Next, Replica)
    end.

stabilize({_, Spid}) ->
    Spid ! {request, self()}.

stabilize(Pred, MyKey, Successor, Next) ->
    {Skey, Sref, Spid} = Successor,
    case Pred of
        nil ->
            Spid ! {notify, {MyKey, self()}}, %% TODO
            {Successor, Next};
        {MyKey, _} ->
            {Successor, Next};
        {Skey, _} ->
            Spid ! {notify, {MyKey, self()}}, %% TODO
            {Successor, Next};
        {Xkey, Xpid} ->
            case key:between(Xkey, MyKey, Skey) of
                true ->
                    self() ! stabilize, %% TODO
                    demonit(Sref),      %% TODO
                    Xref = monit(Xpid), %% TODO
                    {{Xkey, Xref, Xpid}, Successor};  %% TODO
                false ->
                    Spid ! {notify, {MyKey, self()}}, %% TODO
                    {Successor, Next}
            end
    end.

request(Peer, Predecessor, Successor) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil, Successor};
        {Pkey, _Pref, Ppid} ->
            Peer ! {status, {Pkey, Ppid}, Successor}
    end.

notify({Nkey, Npid}, MyKey, Predecessor, Store) ->
    case Predecessor of
        nil ->
            ?DBG(MyKey,"[Notify] New Predecessor:",Nkey),
            Nref = monit(Npid),
            Keep = handover(Store, MyKey, Nkey, Npid),
            {{Nkey, Nref, Npid}, Keep}; %% TODO
        {Pkey, Pref, _} ->
            case key:between(Nkey, Pkey, MyKey) of
                true ->
                    ?DBG(MyKey,"[Notify] New Predecessor:",Nkey),
                    demonit(Pref),
                    Nref = monit(Npid),
                    Keep = handover(Store, MyKey, Nkey, Npid), %% TODO
                    {{Nkey, Nref, Npid}, Keep}; %% TODO
                false ->
                    ?DBG(MyKey,"[Notify] Kept existing predecessor:",Nkey),
                    {Predecessor, Store}
            end
    end.

create_probe(MyKey, {_, Spid}) ->
    Spid ! {probe, MyKey, [MyKey], erlang:now()},
    io:format("Create probe ~w!~n", [MyKey]).

remove_probe(MyKey, Nodes, T) ->
    Time = timer:now_diff(erlang:now(), T),
    io:format("Received probe ~w in ~w ms Ring: ~w~n", [MyKey, Time, Nodes]).

forward_probe(RefKey, Nodes, T, {_, Spid}) ->
    Spid ! {probe, RefKey, Nodes, T},
    io:format("Forward probe ~w!~n", [RefKey]).

add(Key, Value, Qref, Client, MyKey, {Pkey, _}, {_, Spid}, Store) ->
    case key:between(Key, Pkey, MyKey) of %% TODO
        true ->
            Added = storage:add(Key, Value, Store), %% TODO
            Client ! {Qref, ok},
            Spid ! {replicate, Key, Value}, %% sending replica
            Added;
        false ->
            Spid! {add, Key, Value, Qref, Client}, %% TODO
            Store
    end.

lookup(Key, Qref, Client, MyKey, {Pkey, _}, {_, Spid}, Store) ->
    case key:between(Key, Pkey, MyKey) of %% TODO
        true ->
            Result = storage:lookup(Key, Store), %% TODO
            Client ! {Qref, Result};
        false ->
            Spid ! {lookup, Key, Qref, Client} %% TODO
    end.

handover(Store, Replica, MyKey, Nkey, Npid) ->
    {KeepStore, LeaveStore} = storage:split(MyKey, Nkey, Store),
    {KeepRepl, LeaveRepl} = storage:split(MyKey, Nkey, Replica),
    Npid ! {handover, LeaveStore, LeaveRepl},
    {KeepStore, KeepRepl}.

monit(Pid) ->
    erlang:monitor(process, Pid).

demonit(nil) ->
    ok;

demonit(MonitorRef) ->
    erlang:demonitor(MonitorRef, [flush]).

down(Ref, {_, Ref, _}, Successor, Next, Store, Replica) ->
    NewStore = storage:merge(Store, Replica),
    {nil, Successor, Next, NewStore, []};

down(Ref, Predecessor, {_, Ref, _}, {Nkey, Npid}, Store, Replica) ->
    Nref = monit(Npid), %% TODO
    self() ! stabilize, %% TODO
    lists:map(
      fun({Key,Value}) ->
              %% need a smarter way to do this... I don't like it.
              %% we don't know when the successor of a dead node will
              %% be notified, with it these replicate messages might
              %% add values that will be later discarded. Sigh... -_-
              Npid ! {replicate, Key, Value}
      end,
      Store),
    {Predecessor, {Nkey, Nref, Npid}, nil, Store, Replica}.
