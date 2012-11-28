-module(store).
-export([new/1, stop/1, lookup/2]).

new(N) ->
    list_to_tuple(entries(N, [])).

stop(Store) ->
    lists:map(fun(E) ->
                      E ! stop
              end,
              tuple_to_list(Store)).

lookup(I, Store) ->
    element(I, Store). % this is a builtin function

entries(N, Sofar) ->
    if
        N == 0 ->
            Sofar;
        true ->
            Entry = entry:new(0),
            entries(N-1,[Entry|Sofar])
    end.
