-module(acceptor).
-export([start/3]).

-ifdef(debug).
-define(DBG(X,Y,Z), io:format("[Accp_DEBUG] ~w: ~s ~w~n", [X, Y, Z])).
-else.
-define(DBG(X,Y,Z), true).
-endif.

-define(delay, 5000).

start(Name, Seed, PanelId) ->
    spawn(fun() -> init(Name, Seed, PanelId) end).

init(Name, Seed, na) ->
    ?DBG(Name,"Recovering from crash! PID:",self()),
    random:seed(Seed, Seed, Seed),
    {Promise, Voted, Accepted, PanelId} = pers:read(Name),
    acceptor(Name, Promise, Voted, Accepted, PanelId);
init(Name, Seed, PanelId) ->
    ?DBG(Name,"Acceptor Starting! PID:",self()),
    random:seed(Seed, Seed, Seed),
    {Promise, Voted, Accepted, _PanelId} = pers:read(Name),
    acceptor(Name, Promise, Voted, Accepted, PanelId).

acceptor(Name, Promise, Voted, Accepted, PanelId) ->
    receive
        {prepare, Proposer, Round} ->
            ?DBG(Name,"Received a prepare from Proposer",Proposer),
            ?DBG(Name,"And in the Round",Round),
            ?DBG(Name,"I made the following promise so far:",Promise),
            case order:gr(Round, Promise) of
                true ->
                    ?DBG(Name,
                         "New promise. Saving current state in Round",
                         Round),
                    pers:store(Name, Round, Voted, Accepted, PanelId),
                    R = random:uniform(?delay),
                    timer:sleep(R),
                    Proposer ! {promise, Round, Voted, Accepted},
                    % Update gui
                    if
                        Accepted == na ->
                            io:format("[Acceptor ~w] set gui: voted ~w promise ~w colour na~n",
                                      [Name, Voted, Round]),
                            PanelId ! {updateAcc, "Round voted: "
                                       ++ lists:flatten(io_lib:format("~p", [Voted])), "Cur. Promise: "
                                       ++ lists:flatten(io_lib:format("~p", [Round])), {0,0,0}};
                        true ->
                            io:format("[Acceptor ~w] set gui: voted ~w promise ~w colour ~w~n",
                                      [Name, Voted, Round, Accepted]),
                            PanelId ! {updateAcc, "Round voted: "
                                       ++ lists:flatten(io_lib:format("~p", [Voted])), "Cur. Promise: "
                                       ++ lists:flatten(io_lib:format("~p", [Round])), Accepted}
                    end,
                    acceptor(Name, Round, Voted, Accepted, PanelId);
                false ->
                    R = random:uniform(?delay),
                    timer:sleep(R),
                    Proposer ! {sorry, Round},
                    acceptor(Name, Promise, Voted, Accepted, PanelId)
            end;
        {accept, Proposer, Round, Proposal} ->
            ?DBG(Name,"Received an accept message from",Proposer),
            case order:goe(Round, Promise) of
                true ->
                    R = random:uniform(?delay),
                    timer:sleep(R),
                    Proposer ! {vote, Round},
                    case order:goe(Round, Voted) of
                        true ->
                            pers:store(Name, Promise, Round, Proposal, PanelId),
                            % Update gui
                            io:format("[Acceptor ~w] set gui: voted ~w promise ~w colour ~w~n",
                                      [Name, Round, Promise, Proposal]),
                            PanelId ! {updateAcc, "Round voted: "
                                       ++ lists:flatten(io_lib:format("~p", [Round])), "Cur. Promise: "
                                       ++ lists:flatten(io_lib:format("~p", [Promise])), Proposal},
                            acceptor(Name, Promise, Round, Proposal, PanelId);
                        false ->
                            % Update gui
                            io:format("[Acceptor ~w] set gui: voted ~w promise ~w colour ~w~n",
                                      [Name, Round, Promise, Accepted]),
                            PanelId ! {updateAcc, "Round voted: "
                                       ++ lists:flatten(io_lib:format("~p", [Round])), "Cur. Promise: "
                                       ++ lists:flatten(io_lib:format("~p", [Promise])), Accepted},
                            acceptor(Name, Promise, Voted, Accepted, PanelId)
                    end;
                false ->
                    R = random:uniform(?delay),
                    timer:sleep(R),
                    Proposer ! {sorry, Round},
                    acceptor(Name, Promise, Voted, Accepted, PanelId)
            end;
        stop ->
            ?DBG(Name,"Stop received. Should delete the file now.",ok),
            pers:delete(Name),
            ok
    end.

