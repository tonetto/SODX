-module(validator).
-export([start/0]).

start() ->
    spawn_link(fun() -> init() end).

init()->
    validator().

validator() ->
    receive
        {validate, Ref, Reads, Writes, Client} ->
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
    lists:map(fun({_, Entry, Value}) ->
                      %% TODO: ADD SOME CODE
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

check_reads(N, Tag) ->
    if
        N == 0 ->
            ok;
        true ->
            receive
                {Tag, ok} ->
                    check_reads(N-1, Tag);
                {Tag, abort} ->
                    abort
            end
    end.
