-module(node3).
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
            node(MyKey, Predecessor, Successor, [], nil);
        _ ->
            ?DBG(MyKey,"Something wrong on the connect function!",init_fail)
    end.

connect(MyKey, nil) ->
    {ok, {MyKey , nil, self()}}; %% TODO

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

node(MyKey, Predecessor, Successor, Store, Next) ->
    receive
        {key, Qref, PeerPid} ->
            PeerPid ! {Qref, MyKey},
            node(MyKey, Predecessor, Successor, Store, Next);
        {notify, New} ->
            {Pred, NewStore} = notify(New, MyKey, Predecessor, Store),
            node(MyKey, Pred, Successor, NewStore, Next);
        {request, Peer} ->
            request(Peer, Predecessor, Successor),
            node(MyKey, Predecessor, Successor, Store, Next);
        {status, Pred, Nx} ->
            {Succ, Nxt} = stabilize(Pred, MyKey, Successor, Nx),
            node(MyKey, Predecessor, Succ, Store, Nxt);
        stabilize ->
            ?DBG(MyKey,"Starting a new Stabilize",stabilize),
            stabilize(Successor),
            node(MyKey, Predecessor, Successor, Store, Next);
        probe ->
            ?DBG(MyKey,"Starting a probe.",probe),
            create_probe(MyKey, Successor),
            node(MyKey, Predecessor, Successor, Store, Next);
        {probe, MyKey, Nodes, T} ->
            ?DBG(MyKey,"Got my probe back. Ring should be ok.", probe),
            remove_probe(MyKey, Nodes, T),
            node(MyKey, Predecessor, Successor, Store, Next);
        {probe, RefKey, Nodes, T} ->
            ?DBG(MyKey,"Got another probe, forwarding.",probe),
            forward_probe(RefKey, [MyKey|Nodes], T, Successor),
            node(MyKey, Predecessor, Successor, Store, Next);
        {add, Key, Value, Qref, Client} ->
            Added = add(Key, Value, Qref, Client,
                        MyKey, Predecessor, Successor, Store),
            node(MyKey, Predecessor, Successor, Added, Next);
        {lookup, Key, Qref, Client} ->
            lookup(Key, Qref, Client, MyKey, Predecessor, Successor, Store),
            node(MyKey, Predecessor, Successor, Store, Next);
        {handover, Elements} ->
            Merged = storage:merge(Store, Elements),
            node(MyKey, Predecessor, Successor, Merged, Next);
        {'DOWN', Ref, process, _, _} ->
            ?DBG(MyKey,"Node crashed: ",Ref),
            {Pred, Succ, Nxt} = down(Ref, Predecessor, Successor, Next),
            node(MyKey, Pred, Succ, Store, Nxt);
        _ ->
            node(MyKey, Predecessor, Successor, Store, Next)
    end.

stabilize({_, _, Spid}) ->
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
                    {{Xkey, Xref, Xpid}, {Skey, Spid}};  %% TODO
                false ->
                    Spid ! {notify, {MyKey, self()}}, %% TODO
                    {Successor, Next}
            end
    end.

request(Peer, Predecessor, {Skey, _, Spid}) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil, {Skey, Spid}};
        {Pkey, _Pref, Ppid} ->
            Peer ! {status, {Pkey, Ppid}, {Skey, Spid}}
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

create_probe(MyKey, {_, _, Spid}) ->
    Spid ! {probe, MyKey, [MyKey], erlang:now()},
    io:format("Create probe ~w!~n", [MyKey]).

remove_probe(MyKey, Nodes, T) ->
    Time = timer:now_diff(erlang:now(), T),
    io:format("Received probe ~w in ~w ms Ring: ~w~n", [MyKey, Time, Nodes]).

forward_probe(RefKey, Nodes, T, {_, _, Spid}) ->
    Spid ! {probe, RefKey, Nodes, T},
    io:format("Forward probe ~w!~n", [RefKey]).

add(Key, Value, Qref, Client, MyKey, {Pkey, _, _}, {_, _, Spid}, Store) ->
    case key:between(Key, Pkey, MyKey) of %% TODO
        true ->
            Added = storage:add(Key, Value, Store), %% TODO
            Client ! {Qref, ok},
            Added;
        false ->
            Spid! {add, Key, Value, Qref, Client}, %% TODO
            Store
    end.

lookup(Key, Qref, Client, MyKey, {Pkey, _, _}, {_, _, Spid}, Store) ->
    case key:between(Key, Pkey, MyKey) of %% TODO
        true ->
            Result = storage:lookup(Key, Store), %% TODO
            Client ! {Qref, Result};
        false ->
            Spid ! {lookup, Key, Qref, Client} %% TODO
    end.

handover(Store, MyKey, Nkey, Npid) ->
    {Keep, Leave} = storage:split(MyKey, Nkey, Store),
    Npid ! {handover, Leave},
    Keep.

monit(Pid) ->
    erlang:monitor(process, Pid).

demonit(nil) ->
    ok;

demonit(MonitorRef) ->
    erlang:demonitor(MonitorRef, [flush]).

down(Ref, {_, Ref, _}, Successor, Next) ->
    {nil, Successor, Next};

down(Ref, Predecessor, {_, Ref, _}, {Nkey, Npid}) ->
    Nref = monit(Npid), %% TODO
    self() ! stabilize, %% TODO
    {Predecessor, {Nkey, Nref, Npid}, nil}.
