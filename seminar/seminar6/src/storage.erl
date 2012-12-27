-module(storage).
-export([create/0, add/3, lookup/2, merge/2, split/3]).

create() ->
    [].

add(Key, Value, Store) ->
    case lists:keysearch(Key, 1, Store) of
        {value, {_, _}} ->
            io:format("[Store:Add] Key ~w already exists!~n", [Key]),
            Store;
        false ->
            [{Key, Value}|Store]
    end.

lookup(Key, Store) ->
    case lists:keysearch(Key, 1, Store) of
        {value, {Key, Value}} ->
            Value;
        false ->
            io:format("[Store:Lookup] Key ~w does not exist!~n", [Key]),
            -1
    end.

merge(S1, S2) ->
    lists:keymerge(1, S1, S2).

split(Id, Key, Store) ->
    lists:partition(fun({K,_}) -> key:between(K, Key, Id) end, Store).
