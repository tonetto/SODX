-module(paxy_proposers).
-export([start/3]).
-define(RED, {255,0,0}).
-define(BLUE, {0,0,255}).
-define(GREEN, {0,255,0}).

-ifdef(debug).
-define(DBG(X,Y), io:format("[paxy_proposers DEBUG] ~s ~w~n", [X, Y])).
-else.
-define(DBG(X,Y), true).
-endif.

start(Seed, Host, AccptorsHost) ->
    register(proposers, self()),
    ProposerNames = ["Proposer 1", "Proposer 2", "Proposer 3"],
    PropInfo = [{kurtz, ?RED, 10}, {kilgore, ?GREEN, 2},
                {willard, ?BLUE, 3}],
    init_paxy_proposers(ProposerNames, PropInfo, Seed, Host, {acceptors,AccptorsHost}).

init_paxy_proposers(ProposerNames, PropInfo, Seed, Host, Acceptors) ->
    ?DBG("Entering the init_paxy_proposers",ok),
    Acceptors ! {paxy_proposers, ProposerNames, {proposers, Host}},
    receive
        {paxy_acceptors_gui, AccRegister, PropIds} ->
            ?DBG("Received the message back from the Acceptors process.",ok),
            start_proposers(PropIds, PropInfo, AccRegister, Seed)
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
