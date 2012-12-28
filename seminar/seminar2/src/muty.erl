-module(muty).
-export([start/3, stop/0]).

start(Lock, Sleep, Work) ->
    register(l1, spawn(Lock, init,[1, [l2,l3,l4]])),
    register(l2, spawn(Lock, init,[2, [l1,l3,l4]])),
    register(l3, spawn(Lock, init,[3, [l1,l2,l4]])),
    register(l4, spawn(Lock, init,[4, [l1,l2,l3]])),
    register(john,   spawn(worker, init, ["John", l1,34,Sleep,Work])),
    register(ringo,  spawn(worker, init, ["Ringo", l2,37,Sleep,Work])),
    register(paul,   spawn(worker, init, ["Paul", l3,43,Sleep,Work])),
    register(george, spawn(worker, init, ["George",l4,72,Sleep,Work])),
    ok.

stop() ->
    john ! stop,
    ringo ! stop,
    paul ! stop,
    george ! stop,
    l1 ! stop,
    l2 ! stop,
    l3 ! stop,
    l4 ! stop.
