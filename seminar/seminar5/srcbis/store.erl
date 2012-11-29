-module(store).
-export([new/1, stop/1, lookup/2]).

-ifdef(debug_store).
-define(DBG(X,Y,Z), io:format("[STORE_DEBUG] ~w: ~s ~w~n", [X, Y, Z])).
-else.
-define(DBG(X,Y,Z), true).
-endif.

new(N) ->
    list_to_tuple(entries(N, [])).

stop(Store) ->
    lists:map(fun(E) ->
                      E ! stop
              end,
              tuple_to_list(Store)).

lookup(I, Store) ->
    ?DBG(self(),"Lookup:",{I,Store}),
    element(I, Store). % this is a builtin function

entries(N, Sofar) ->
    if
        N == 0 ->
            Sofar;
        true ->
            Entry = entry:new(0),
            entries(N-1,[Entry|Sofar])
    end.
