-module(acceptor).
-export([start/3]).

-ifdef(debug).
-define(DBG(X,Y,Z), io:format("[Accp_DEBUG] ~w: ~s ~w~n", [X, Y, Z])).
-else.
-define(DBG(X,Y,Z), true).
-endif.

-define(delay, 500).

start(Name, Seed, PanelId) ->
    spawn(fun() -> init(Name, Seed, PanelId) end).

init(Name, Seed, PanelId) ->
    ?DBG(Name,"Acceptor Starting! PID:",self()),
    random:seed(Seed, Seed, Seed),
    Promise = order:null(),
    Voted = order:null(),
    Accepted = na,
    acceptor(Name, Promise, Voted, Accepted, PanelId).

acceptor(Name, Promise, Voted, Accepted, PanelId) ->
    receive
        {prepare, Proposer, Round} ->
            ?DBG(Name,"Received a prepare from Proposer",Proposer),
            ?DBG(Name,"And in the Round",Round),
            ?DBG(Name,"I made the following promise so far:",Promise),
            case order:gr(Round, Promise) of
                true ->
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
                    ?DBG(Name,"Ignoring sorry for prepare on Round",Round),
                    acceptor(Name, Promise, Voted, Accepted, PanelId)
            end;
        {accept, Proposer, Round, Proposal} ->
            ?DBG(Name,"Received an accept message from",Proposer),
            ?DBG(Name,"For voting in the round",Round),
            ?DBG(Name,"For voting in the value",Proposal),
            ?DBG(Name,"My promise so far is",Promise),
            ?DBG(Name,"The highest ballot I voted for is",Voted),
            case order:goe(Round, Promise) of
                true ->
                    R = random:uniform(?delay),
                    timer:sleep(R),
                    Proposer ! {vote, Round},
                    case order:goe(Round, Voted) of
                        true ->
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
                    ?DBG(Name,"Ignoring sorry for accept for accept on Round",Round),
                    acceptor(Name, Promise, Voted, Accepted, PanelId)
            end;
        stop ->
            ok
    end.

