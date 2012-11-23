-module(server).
-export([start/1]).

-ifdef(debug).
-define(DBG(X,Y,Z), io:format("[SERVER_DEBUG] ~w: ~s ~w~n", [X, Y, Z])).
-else.
-define(DBG(X,Y,Z), true).
-endif.

start(N) ->
    spawn(fun() -> init(N) end).

init(N) ->
    ?DBG(server,"Initilizing the server for N",N),
    Store = store:new(N),
    Validator = validator:start(),
    server(Validator, Store).

server(Validator, Store) ->
    receive
        {open, Client} ->
            ?DBG(server,"Received an open from Client",Client),
            Client ! {transaction, Validator, Store},
            server(Validator, Store);
        stop ->
            store:stop(Store)
    end.
