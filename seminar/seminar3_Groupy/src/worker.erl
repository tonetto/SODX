-module(worker).
-export([start/4, start/5]).
-define(change, 20).
-define(color, {0,0,0}).

start(Id, Module, Rnd, Sleep) ->
    spawn(fun() -> init(Id, Module, Rnd, Sleep) end).

init(Id, Module, Rnd, Sleep) ->
    Cast = apply(Module, start, [Id]),
    Color = ?color,
    init_cont(Id, Rnd, Cast, Color, Sleep).

start(Id, Module, Rnd, Peer, Sleep) ->
    spawn(fun() -> init(Id, Module, Rnd, Peer, Sleep) end).

init(Id, Module, Rnd, Peer, Sleep) ->
    Cast = apply(Module, start, [Id, Peer]),
    receive
        {ok, Color} ->
            init_cont(Id, Rnd, Cast, Color, Sleep);
        {error, Error} ->
            io:format("error: ~s~n", [Error])
    end.

init_cont(Id, Rnd, Cast, Color, Sleep) ->
    random:seed(Rnd, Rnd, Rnd),
    Gui = gui:start(Id, self()),
    Gui ! {color, Color},
    worker(Id, Cast, Color, Gui, Sleep),
    Cast ! stop,
    Gui ! stop.

worker(Id, Cast, Color, Gui, Sleep) ->
    Wait = if Sleep == 0 -> 0; true -> random:uniform(Sleep) end,
    receive
        {deliver, {_From, N}} ->
            Color2 = change_color(N, Color),
            Gui ! {color, Color2},
            worker(Id, Cast, Color2, Gui, Sleep);
        {join, Peer} ->
            Cast ! {join, Peer},
            worker(Id, Cast, Color, Gui, Sleep);
        request ->
            Cast ! {ok, Color},
            worker(Id, Cast, Color, Gui, Sleep);
        stop ->
            ok;
        Error ->
            io:format("strange message: ~w~n", [Error]),
            worker(Id, Cast, Color, Gui, Sleep)
    after Wait ->
            Cast ! {mcast, {Id, random:uniform(?change)}},
            worker(Id, Cast, Color, Gui, Sleep)
    end.

change_color(N, {R,G,B}) ->
    {G, B, ((R+N) rem 256)}.
