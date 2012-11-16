-module(paxy).
-export([start/1, stop/0, stop/1]).
-define(RED, {255,0,0}).
-define(BLUE, {0,0,255}).
-define(GREEN, {0,255,0}).
-define(GREY, {84,84,84}).
-define(MARINE, {112,219,147}).
-define(PURPLE, {131,111,255}).
-define(CYAN, {0,255,255}).
-define(BROWN, {92,64,51}).
-define(BEIGE, {255,211,155}).
-define(ORANGE, {255,127,36}).

start(Seed) ->
    AcceptorNames = ["Acceptor 1", "Acceptor 2", "Acceptor 3",
                     "Acceptor 4", "Acceptor 5"],
    AccRegister = [a, b, c, d, e],
    ProposerNames = ["Proposer 1", "Proposer 2", "Proposer 3",
                     "Proposer 4", "Proposer 5", "Proposer 6",
                     "Proposer 7", "Proposer 8", "Proposer 9",
                     "Proposer 10"],
    PropInfo = [{kurtz, ?RED, 2}, {kilgore, ?GREEN, 3},
                {willard, ?BLUE, 5}, {leo, ?GREY, 7},
                {peter, ?MARINE, 11}, {roshan, ?PURPLE, 13},
                {casey, ?CYAN, 17}, {xela, ?BROWN, 19},
                {ale, ?BEIGE, 23}, {ernesto, ?ORANGE, 29}],
    
    % computing panel heights
    AccPanelHeight = length(AcceptorNames)*50 + 20, %plus the spacer value
    PropPanelHeight = length(ProposerNames)*50 + 20,
    register(gui, spawn(fun() -> gui:start(AcceptorNames, ProposerNames,
                                           AccPanelHeight, PropPanelHeight) end)),
    gui ! {reqState, self()},
    receive
        {reqState, State} ->
            {AccIds, PropIds} = State,
            start_acceptors(AccIds, AccRegister, Seed),
            start_proposers(PropIds, PropInfo, AccRegister, Seed+1)
    end,
    true.

start_acceptors(AccIds, AccReg, Seed) ->
    case AccIds of
        [] ->
            ok;
        [AccId|Rest] ->
            [RegName|RegNameRest] = AccReg,
            register(RegName, acceptor:start(RegName, Seed, AccId)),
            start_acceptors(Rest, RegNameRest, Seed+1)
    end.

start_proposers(PropIds, PropInfo, Acceptors, Seed) ->
    case PropIds of
        [] ->
            ok;
        [PropId|Rest] ->
            [{RegName, Colour, Inc}|RestInfo] = PropInfo,
            proposer:start(RegName, Colour, Acceptors, Seed+Inc, PropId),
            start_proposers(Rest, RestInfo, Acceptors, Seed)
    end.

stop() ->
    stop(gui),
    stop(a),
    stop(b),
    stop(c),
    stop(d),
    stop(e).

stop(Name) ->
    case whereis(Name) of
        undefined ->
            ok;
        Pid ->
            Pid ! stop
    end.
