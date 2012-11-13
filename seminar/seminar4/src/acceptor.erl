-module(acceptor).
-export([start/3]).

start(Name, Seed, PanelId) ->
    spawn(fun() -> init(Name, Seed, PanelId) end).

init(Name, Seed, PanelId) ->
    random:seed(Seed, Seed, Seed),
    Promise = order:null(),
    Voted = order:null(),
    Accepted = na,
    acceptor(Name, Promise, Voted, Accepted, PanelId).

acceptor(Name, Promise, Voted, Accepted, PanelId) ->
    receive
        {prepare, Proposer, Round} ->
            case order:gr(Round, Promise) of
                true ->
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
                    Proposer ! {sorry, Round},
                    acceptor(Name, Promise, Voted, Accepted, PanelId)
            end;
        {accept, Proposer, Round, Proposal} ->
            case order:goe(Round, Promise) of
                true ->
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
                    Proposer ! {sorry, Round},
                    acceptor(Name, Promise, Voted, Accepted, PanelId)
            end;
        stop ->
            ok
    end.

