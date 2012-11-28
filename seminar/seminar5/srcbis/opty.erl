-module(opty).
-export([start/5, stop/1, starttests/0]).

start(Clients, Entries, Updates, Time, Round) ->
    register(s, server:start(Entries)),
    L = startClients(Clients, [], Entries, Updates, Round),
    io:format("Starting: ~w CLIENTS, ~w ENTRIES, " ++
              "~w UPDATES PER TRANSACTION, DURATION ~w s ~n",
              [Clients, Entries, Updates, Time]),
    timer:sleep(Time*1000),
    stop(L).

starttests() ->
    register(stat, statistic:start()),
    %%       {Clients, Entries, Updates, Time, Round}
    Tests = [{      8,       1,       4,   10,     7},
             {      8,       2,       4,   10,     7},
             {      8,       4,       4,   10,     7},
             {      8,       6,       4,   10,     8},
             {      8,       8,       4,   10,     9},
             {      8,      16,       4,   10,    10},
             {      8,      24,       4,   10,    11},
             {      8,      32,       4,   10,    12},
             {      8,      48,       4,   10,    13},
             {      8,      64,       4,   10,    14},
             {      8,      96,       4,   10,    15},
             {      8,     128,       4,   10,    16}],
    lists:map(fun(T) ->
                      {Clients, Entries, Updates, Time, Round} = T,
                      start(Clients, Entries, Updates, Time, Round),
                      %% Small sleep to avoid CPU overheating
                      timer:sleep(Time*500)
              end,
              Tests),
    timer:sleep(5000),
    stat ! stop.

stop(L) ->
    io:format("Stopping...~n"),
    stopClients(L),
    s ! stop,
    unregister(s).

startClients(0,L,_,_,_) -> L;

startClients(Clients, L, Entries, Updates, Round) ->
    Pid = client:start(Clients, Entries, Updates, s, Round),
    startClients(Clients-1, [Pid|L], Entries, Updates, Round).

stopClients([]) -> ok;

stopClients([Pid|L]) ->
    Pid ! {stop, self()},
    receive
        {done, Pid} -> ok
    end,
    stopClients(L).
