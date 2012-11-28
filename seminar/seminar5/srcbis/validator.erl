-module(validator).
-export([start/0]).

-ifdef(debug).
-define(DBG(X,Y,Z), io:format("[VALIDATOR_DEBUG] ~w: ~s ~w~n", [X, Y, Z])).
-else.
-define(DBG(X,Y,Z), true).
-endif.

start() ->
    spawn_link(fun() -> init() end).

init()->
    ?DBG(validator,"Initializing validator",ok),
    validator().

validator() ->
    receive
        {validate, Ref, Reads, Writes, Client} ->
            ?DBG(validator,"Received a validate from client",Client),
            case validate(Reads) of
                ok ->
                    update(Writes),
                    Client ! {Ref, ok};
                abort ->
                    Client ! {Ref, abort}
            end,
            validator();
        _Old ->
            validator()
    end.

update(Writes) ->
    ?DBG(validator,"Commiting writes!",ok),
    lists:map(fun({_, Entry, Value}) ->
                      Entry ! {write, Value}
              end,
              Writes).

validate(Reads) ->
    {N, Tag} = send_checks(Reads),
    check_reads(N, Tag).

send_checks(Reads) ->
    Tag = make_ref(),
    Self = self(),
    N = length(Reads),
    lists:map(fun({Entry, Time}) ->
                      Entry ! {check, Tag, Time, Self}
              end,
              Reads),
    {N, Tag}.

check_reads(0, Tag) ->
    ?DBG(validator,"All reads were ok for Tag",Tag),
    ok;

check_reads(N, Tag) ->
    receive
        {Tag, ok} ->
            check_reads(N-1, Tag);
        {Tag, abort} ->
            ?DBG(validator,"Conflicting reads for Tag",Tag),
            abort
    end.
