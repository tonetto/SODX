-module(groupy).
-export([start/1, stop/0]).
start(Module) ->
    Leader = worker:start("1", Module, 1, 1000),
    register(a, Leader),
    register(b, worker:start("2", Module, 2, Leader, 1000)),
    register(c, worker:start("3", Module, 3, Leader, 1000)),
    register(d, worker:start("4", Module, 4, Leader, 1000)),
    register(e, worker:start("5", Module, 5, Leader, 1000)).

stop() ->
    a ! stop,
    b ! stop,
    c ! stop,
    d ! stop,
    e ! stop.
