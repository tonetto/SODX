-module(node2).
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
            node(MyKey, Predecessor, Successor, []);
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
            {ok, {Skey, PeerPid}} %% TODO
    after ?Timeout ->
            io:format("Timeout: no response from ~w~n", [PeerPid])
    end.

schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

node(MyKey, Predecessor, Successor, Store) ->
    receive
        {key, Qref, PeerPid} ->
            PeerPid ! {Qref, MyKey},
            node(MyKey, Predecessor, Successor, Store);
        {notify, New} ->
            {Pred, NewStore} = notify(New, MyKey, Predecessor, Store),
            node(MyKey, Pred, Successor, NewStore);
        {request, Peer} ->
            request(Peer, Predecessor),
            node(MyKey, Predecessor, Successor, Store);
        {status, Pred} ->
            Succ = stabilize(Pred, MyKey, Successor),
            node(MyKey, Predecessor, Succ, Store);
        stabilize ->
            ?DBG(MyKey,"Starting a new Stabilize",stabilize),
            stabilize(Successor),
            node(MyKey, Predecessor, Successor, Store);
        probe ->
            ?DBG(MyKey,"Starting a probe.",probe),
            create_probe(MyKey, Successor),
            node(MyKey, Predecessor, Successor, Store);
        {probe, MyKey, Nodes, T} ->
            ?DBG(MyKey,"Got my probe back. Ring should be ok.", probe),
            remove_probe(MyKey, Nodes, T),
            node(MyKey, Predecessor, Successor, Store);
        {probe, RefKey, Nodes, T} ->
            ?DBG(MyKey,"Got another probe, forwarding.",probe),
            forward_probe(RefKey, [MyKey|Nodes], T, Successor),
            node(MyKey, Predecessor, Successor, Store);
        {add, Key, Value, Qref, Client} ->
            Added = add(Key, Value, Qref, Client,
                        MyKey, Predecessor, Successor, Store),
            node(MyKey, Predecessor, Successor, Added);
        {lookup, Key, Qref, Client} ->
            lookup(Key, Qref, Client, MyKey, Predecessor, Successor, Store),
            node(MyKey, Predecessor, Successor, Store);
        {handover, Elements} ->
            Merged = storage:merge(Store, Elements),
            node(MyKey, Predecessor, Successor, Merged);
        _ ->
            node(MyKey, Predecessor, Successor, Store)
    end.

stabilize({_, Spid}) ->
    Spid ! {request, self()}.

stabilize(Pred, MyKey, Successor) ->
    {Skey, Spid} = Successor,
    case Pred of
        nil ->
            Spid ! {notify, {MyKey, self()}}, %% TODO
            Successor;
        {MyKey, _} ->
            Successor;
        {Skey, _} ->
            Spid ! {notify, {MyKey, self()}}, %% TODO
            Successor;
        {Xkey, Xpid} ->
            case key:between(Xkey, MyKey, Skey) of
                true ->
                    self() ! stabilize, %% TODO
                    {Xkey, Xpid}; %% TODO
                false ->
                    Spid ! {notify, {MyKey, self()}}, %% TODO
                    Successor
            end
    end.

request(Peer, Predecessor) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil};
        {Pkey, Ppid} ->
            Peer ! {status, {Pkey, Ppid}}
    end.

notify({Nkey, Npid}, MyKey, Predecessor, Store) ->
    case Predecessor of
        nil ->
            ?DBG(MyKey,"[Notify] New Predecessor:",Nkey),
            Keep = handover(Store, MyKey, Nkey, Npid),
            {{Nkey, Npid}, Keep}; %% TODO
        {Pkey, _} ->
            case key:between(Nkey, Pkey, MyKey) of
                true ->
                    ?DBG(MyKey,"[Notify] New Predecessor:",Nkey),
                    Keep = handover(Store, MyKey, Nkey, Npid), %% TODO
                    {{Nkey, Npid}, Keep}; %% TODO
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

handover(Store, MyKey, Nkey, Npid) ->
    {Keep, Leave} = storage:split(MyKey, Nkey, Store),
    Npid ! {handover, Leave},
    Keep.
