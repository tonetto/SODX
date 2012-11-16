-module(proposer).
-export([start/5]).
-define(timeout, 2000).
-define(backoff, 10).

-ifdef(debug).
-define(DBG(X,Y,Z), io:format("[Prop_DEBUG] ~w: ~s ~w~n", [X, Y, Z])).
-else.
-define(DBG(X,Y,Z), true).
-endif.

start(Name, Proposal, Acceptors, Seed, PanelId) ->
    spawn(fun() -> init(Name, Proposal, Acceptors, Seed, PanelId) end).

init(Name, Proposal, Acceptors, Seed, PanelId) ->
    ?DBG(Name,"Proposer starting! PID:",self()),
    random:seed(Seed, Seed, Seed),
    Round = order:null(Name),
    round(Name, ?backoff, Round, Proposal, Acceptors, PanelId).

round(Name, Backoff, Round, Proposal, Acceptors, PanelId) ->
    % Update gui
    io:format("[Proposer ~w | round()] set gui: Round ~w Proposal ~w~n",
              [Name, Round, Proposal]),
    PanelId ! {updateProp, "Round: "
               ++ lists:flatten(io_lib:format("~p", [Round])), "Proposal: "
               ++ lists:flatten(io_lib:format("~p", [Proposal])), Proposal},
    case ballot(Name, Round, Proposal, Acceptors, PanelId) of
        {ok, Decision} ->
            io:format("[Proposer ~w] ~w decided ~w in round ~w~n",
                      [Name, Acceptors, Decision, Round]),
            {ok, Decision};
        abort ->
            ?DBG(Name,"Aborted this round, let's try again! Failed round:",Round),
            timer:sleep(random:uniform(Backoff)),
            Next = order:inc(Round),
            round(Name, (2*Backoff), Next, Proposal, Acceptors, PanelId)
    end.

ballot(Name, Round, Proposal, Acceptors, PanelId) ->
    prepare(Round, Acceptors),
    Quorum = (length(Acceptors) div 2) + 1,
    ?DBG(Name,"Quorum value is:",Quorum),
    Max = order:null(),
    case collect(Name, Quorum, Round, Max, Proposal) of
        {accepted, Value} ->
            % update gui
            io:format("[Proposer ~w | ballot()] set gui: Round ~w Proposal ~w~n",
                      [Name, Round, Value]),
            PanelId ! {updateProp, "Round: "
                       ++ lists:flatten(io_lib:format("~p", [Round])), "Proposal: "
                       ++ lists:flatten(io_lib:format("~p", [Value])), Value},
            accept(Round, Value, Acceptors),
            case vote(Name, Quorum, Round) of
                ok ->
                    {ok, Value};
                abort ->
                    abort
            end;
        abort ->
            abort
    end.

collect(_, 0, _, _, Proposal) ->
    {accepted, Proposal};

collect(Name, N, Round, Max, Proposal) ->
    ?DBG(Name,"promises missing:",N),
    receive
        {promise, Round, _, na} ->
            ?DBG(Name,"Received a promise for Round",Round),
            collect(Name, N-1, Round, Max, Proposal);
        {promise, Round, Voted, Value} ->
            ?DBG(Name,"Received a promise for Round",Round),
            case order:gr(Voted, Max) of
                true ->
                    ?DBG(Name,"New sequence number is",Voted),
                    ?DBG(Name,"New value is",Value),
                    collect(Name, N-1, Round, Voted, Value);
                false ->
                    collect(Name, N-1, Round, Max, Proposal)
            end;
        {promise, _, _, _} ->
            collect(Name, N, Round, Max, Proposal);
        {sorry, Round} ->
            ?DBG(Name,"Received a sorry for Round", Round),
            collect(Name, N, Round, Max, Proposal);
        {sorry, _} ->
            collect(Name, N, Round, Max, Proposal)
    after ?timeout ->
            abort
    end.

vote(_, 0, _) ->
    ok;

vote(Name, N, Round) ->
    ?DBG(Name,"Votes missing:",N),
    receive
        {vote, Round} ->
            ?DBG(Name,"Received one more vote for Round",Round),
            vote(Name, N-1, Round);
        {vote, _} ->
            vote(Name, N, Round);
        {sorry, Round} ->
            ?DBG(Name,"Received one more sorry for Round",Round),
            vote(Name, N, Round);
        {sorry, _} ->
            vote(Name, N, Round)
    after ?timeout ->
            abort
    end.

prepare(Round, Acceptors) ->
    Fun = fun(Acceptor) ->
                  send(Acceptor, {prepare, self(), Round})
          end,
    lists:map(Fun, Acceptors).

accept(Round, Proposal, Acceptors) ->
    Fun = fun(Acceptor) ->
                  send(Acceptor, {accept, self(), Round, Proposal})
          end,
    lists:map(Fun, Acceptors).

send(Name, Message) ->
    Name ! Message.
