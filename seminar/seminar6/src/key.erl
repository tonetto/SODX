-module(key).
-export([generate/0, between/3]).

-define(N, 1000000000).

generate() ->
    random:uniform(?N).

between(Key, From, To) ->
    if
        From == To ->
            true;
        From < To ->
            (From < Key) and (Key =< To);
        From > To ->
            (From < Key) or (Key =< To)
    end.
