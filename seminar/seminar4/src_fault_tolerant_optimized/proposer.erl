-module(proposer).
-export([start/5]).
-define(timeout, 1000).
-define(backoff, 10).

-ifdef(debug).
-define(DBG(X,Y,Z), io:format("[Prop_DEBUG] ~w: ~s ~w~n", [X, Y, Z])).
-else.
-define(DBG(X,Y,Z), true).
-endif.

-define(delay, 850).

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
    case collect(Name, Quorum, Quorum, Round, Max, Proposal) of
        {accepted, Value} ->
            % update gui
            io:format("[Proposer ~w | ballot()] set gui: Round ~w Proposal ~w~n",
                      [Name, Round, Value]),
            PanelId ! {updateProp, "Round: "
                       ++ lists:flatten(io_lib:format("~p", [Round])), "Proposal: "
                       ++ lists:flatten(io_lib:format("~p", [Value])), Value},
            accept(Round, Value, Acceptors),
            case vote(Name, Quorum, Quorum, Round) of
                ok ->
                    {ok, Value};
                abort ->
                    abort
            end;
        abort ->
            abort
    end.

collect(_, 0, _, _, _, Proposal) ->
    {accepted, Proposal};

collect(Name, _, 0, Round, _, _) ->
    ?DBG(Name,"---[Optimization] Aborting collect on Round",Round),
    abort;

collect(Name, N, AbortN, Round, Max, Proposal) ->
    ?DBG(Name,"promises missing:",N),
    receive
        {promise, Round, _, na} ->
            ?DBG(Name,"Received a promise for Round",Round),
            collect(Name, N-1, AbortN, Round, Max, Proposal);
        {promise, Round, Voted, Value} ->
            ?DBG(Name,"Received a promise for Round",Round),
            case order:gr(Voted, Max) of
                true ->
                    ?DBG(Name,"New sequence number is",Voted),
                    ?DBG(Name,"New value is",Value),
                    collect(Name, N-1, AbortN, Round, Voted, Value);
                false ->
                    collect(Name, N-1, AbortN, Round, Max, Proposal)
            end;
        {promise, _, _, _} ->
            collect(Name, N, AbortN, Round, Max, Proposal);
        {sorry, Round} ->
            ?DBG(Name,"Received a sorry for Round", Round),
            collect(Name, N, AbortN-1, Round, Max, Proposal);
        {sorry, _} ->
            collect(Name, N, AbortN, Round, Max, Proposal)
    after ?timeout ->
            abort
    end.

vote(_, 0, _, _) ->
    ok;

vote(Name, _, 0, Round) ->
    ?DBG(Name,"---[Optimization] Abort voting on Round",Round),
    abort;

vote(Name, N, AbortN, Round) ->
    ?DBG(Name,"Votes missing:",N),
    receive
        {vote, Round} ->
            ?DBG(Name,"Received one more vote for Round",Round),
            vote(Name, N-1, AbortN, Round);
        {vote, _} ->
            vote(Name, N, AbortN, Round);
        {sorry, Round} ->
            ?DBG(Name,"Received one more sorry for Round",Round),
            vote(Name, N, AbortN-1, Round);
        {sorry, _} ->
            vote(Name, N, AbortN, Round)
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
    case whereis(Name) of
        undefined ->
            down;
        Pid ->
            R = random:uniform(?delay),
            timer:sleep(R),
            Pid ! Message
    end.
