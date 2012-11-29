-module(opty).
-export([stop/1, starttests/0]).

start(Clients, Entries, Updates, Time, Round, Sleep, Read, Write, Slice) ->
    register(s, server:start(Entries)),
    L = startClients(Clients, [], Entries, Updates, Round, Sleep,
                     Read, Write, Slice),
    io:format("#~w Starting: ~w CLIENTS, ~w ENTRIES, " ++
              "~w UPDATES PER TRANSACTION, DURATION ~w s, " ++
              "TRANSACTION DURATION ~w ms, READ ~w, WRITE ~w, SLICE ~w~n",
              [Round, Clients, Entries, Updates, Time, Sleep,
               Read, Write, Slice]),
    timer:sleep(Time*1000),
    stop(L).

starttests() ->
    register(stat, statistic:start()),
    %%       {Clients, Entries, Updates, Time, Round, Sleep, Read, Write, Slice}
    Tests = [{      8,       1,       4,   20,     1,     0,    1,     1,     1},
             {      8,       2,       4,   20,     2,     0,    1,     1,     1},
             {      8,       4,       4,   20,     3,     0,    1,     1,     1},
             {      8,       6,       4,   20,     4,     0,    1,     1,     1},
             {      8,       8,       4,   20,     5,     0,    1,     1,     1},
             {      8,      16,       4,   20,     6,     0,    1,     1,     1},
             {      8,      24,       4,   20,     7,     0,    1,     1,     1},
             {      8,      32,       4,   20,     8,     0,    1,     1,     1},
             {      8,      48,       4,   20,     9,     0,    1,     1,     1},
             {      8,      64,       4,   20,    10,     0,    1,     1,     1},
             {      8,      96,       4,   20,    11,     0,    1,     1,     1},
             {      8,     128,       4,   20,    12,     0,    1,     1,     1},
             %% Multiple transaction times
             {      8,      48,       4,   20,    13,     0,    1,     1,     1},
             {      8,      48,       4,   20,    14,     1,    1,     1,     1},
             {      8,      48,       4,   20,    15,     2,    1,     1,     1},
             {      8,      48,       4,   20,    16,     3,    1,     1,     1},
             {      8,      48,       4,   20,    17,     4,    1,     1,     1},
             {      8,      48,       4,   20,    18,     5,    1,     1,     1},
             {      8,      48,       4,   20,    19,     7,    1,     1,     1},
             {      8,      48,       4,   20,    20,     9,    1,     1,     1},
             {      8,      48,       4,   20,    21,    10,    1,     1,     1},
             {      8,      48,       4,   20,    22,    14,    1,     1,     1},
             {      8,      48,       4,   20,    23,    18,    1,     1,     1},
             {      8,      48,       4,   20,    24,    30,    1,     1,     1},
             %% Different R/W ratio
             {      8,      48,       4,   20,    25,     0,   11,     0,     1},
             {      8,      48,       4,   20,    26,     0,   10,     1,     1},
             {      8,      48,       4,   20,    27,     0,    9,     2,     1},
             {      8,      48,       4,   20,    28,     0,    8,     3,     1},
             {      8,      48,       4,   20,    29,     0,    7,     4,     1},
             {      8,      48,       4,   20,    30,     0,    6,     5,     1},
             {      8,      48,       4,   20,    31,     0,    5,     6,     1},
             {      8,      48,       4,   20,    32,     0,    4,     7,     1},
             {      8,      48,       4,   20,    33,     0,    3,     8,     1},
             {      8,      48,       4,   20,    34,     0,    2,     9,     1},
             {      8,      48,       4,   20,    35,     0,    1,    10,     1},
             {      8,      48,       4,   20,    36,     0,    0,    11,     1},
             %% Different percentage of modified entries
             {      8,      48,       4,   20,    37,     0,    1,     1,     1},
             {      8,      48,       4,   20,    38,     0,    1,     1,0.9166},
             {      8,      48,       4,   20,    39,     0,    1,     1,0.8333},
             {      8,      48,       4,   20,    40,     0,    1,     1,  0.75},
             {      8,      48,       4,   20,    41,     0,    1,     1,0.6666},
             {      8,      48,       4,   20,    42,     0,    1,     1,0.5833},
             {      8,      48,       4,   20,    43,     0,    1,     1,   0.5},
             {      8,      48,       4,   20,    44,     0,    1,     1,0.4166},
             {      8,      48,       4,   20,    45,     0,    1,     1,0.3333},
             {      8,      48,       4,   20,    46,     0,    1,     1,  0.25},
             {      8,      48,       4,   20,    47,     0,    1,     1,0.1666},
             {      8,      48,       4,   20,    48,     0,    1,     1,0.0833}],
    lists:map(fun(T) ->
                      {Clients, Entries, Updates, Time, Round, Sleep,
                       Read, Write, Slice} = T,
                      start(Clients, Entries, Updates, Time, Round, Sleep,
                            Read, Write, Slice),
                      %% Small sleep to avoid CPU overheating
                      timer:sleep(Time*350)
              end,
              Tests),
    timer:sleep(5000),
    stat ! stop.

stop(L) ->
    io:format("Stopping...~n"),
    stopClients(L),
    s ! stop,
    unregister(s).

startClients(0,L,_,_,_,_,_,_,_) -> L;

startClients(Clients, L, Entries, Updates, Round, Sleep, Read, Write, Slice) ->
    Pid = client:start(Clients, Entries, Updates, s, Round, Sleep,
                       Read, Write, Slice),
    startClients(Clients-1, [Pid|L], Entries, Updates, Round, Sleep,
                 Read, Write, Slice).

stopClients([]) -> ok;

stopClients([Pid|L]) ->
    Pid ! {stop, self()},
    receive
        {done, Pid} -> ok
    end,
    stopClients(L).
