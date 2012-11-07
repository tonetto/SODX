-module(groupybis).
-export([start/1, stop/0]).
start(Module) ->
    %% group 1 (original)
    Leader = worker:start("1", Module, 1, 1000),
    register(a, Leader),
    register(b, worker:start("2", Module, 2, Leader, 1000)),
    register(c, worker:start("3", Module, 3, Leader, 1000)),
    register(d, worker:start("4", Module, 4, Leader, 1000)),
    register(e, worker:start("5", Module, 5, Leader, 1000)),
    %% group 2 (additional)
    Leader2 = worker:start("1a", Module, 1, 1200),
    register(f, Leader2),
    register(g, worker:start("2a", Module, 2, Leader2, 1200)),
    register(h, worker:start("3a", Module, 3, Leader2, 1200)),
    register(i, worker:start("4a", Module, 4, Leader2, 1200)),
    register(j, worker:start("5a", Module, 5, Leader2, 1200)),
    register(k, worker:start("6a", Module, 5, j, 1200)).

stop() ->
    a ! stop,
    b ! stop,
    c ! stop,
    d ! stop,
    e ! stop,
    f ! stop,
    g ! stop,
    h ! stop,
    i ! stop,
    j ! stop,
    k ! stop.
