-module(handler).
-export([start/3]).

-ifdef(debug_handler).
-define(DBG(X,Y,Z), io:format("[HANDLER_DEBUG] ~w: ~s ~w~n", [X, Y, Z])).
-else.
-define(DBG(X,Y,Z), true).
-endif.

start(Client, Validator, Store) ->
    spawn_link(fun() -> init(Client, Validator, Store) end).

init(Client, Validator, Store) ->
    handler(Client, Validator, Store, [], []).

handler(Client, Validator, Store, Reads, Writes) ->
    ?DBG(Client,"Waiting on Handler state",waiting),
    receive
        {read, Ref, N} ->
            case lists:keysearch(N, 1, Writes) of
                {value, {N, _, Value}} ->
                    ?DBG(Client,"Existing read entry",Value),
                    Client ! {value, Ref, Value},
                    handler(Client, Validator, Store, Reads, Writes);
                false ->
                    ?DBG(Client,"New entry requested",N),
                    Entry = store:lookup(N, Store),
                    Entry ! {read, Ref, self()},
                    handler(Client, Validator, Store, Reads, Writes)
            end;
        {Ref, Entry, Value, Time} ->
            ?DBG(Client,"Received the read value from the Entry:",Value),
            Client ! {value, Ref, Value},
            handler(Client, Validator, Store, [{Entry, Time}|Reads], Writes);
        {write, N, Value} ->
            ?DBG(Client,"write received for entry N",N),
            Entry = store:lookup(N, Store),
            Added = lists:keystore(N, 1, Writes,{N, Entry, Value}),
            handler(Client, Validator, Store, Reads, Added);
        {commit, Ref} ->
            ?DBG(Client,"Sending a validate to commit the transactions",Ref),
            Validator ! {validate, Ref, Reads, Writes, Client};
        abort ->
            ?DBG(Client,"abort received!",ok),
            ok
    end.
