-module(paxy_acceptors_gui).
-export([start/2, stop/0, stop/1]).

-ifdef(debug).
-define(DBG(X,Y), io:format("[paxy_acceptors DEBUG] ~s ~w~n", [X, Y])).
-else.
-define(DBG(X,Y), true).
-endif.

start(Seed, Host) ->
    register(acceptors,self()),
    AcceptorNames = ["Acceptor 1", "Acceptor 2", "Acceptor 3",
                     "Acceptor 4", "Acceptor 5"],
    AccRegister = [a, b, c, d, e],
    init_paxy_acceptors(Seed, AcceptorNames, AccRegister, Host).

init_paxy_acceptors(Seed, AcceptorNames, AccRegister, Host) ->
    ?DBG("Entered the init_paxy_acceptors functions.",ok),
    receive
        {paxy_proposers, ProposerNames, ProposersProc} ->
            ?DBG("Got the message from the proposers",ok),
            ?DBG("ProposerProc",ProposersProc),
            % computing panel heights
            AccPanelHeight = length(AcceptorNames)*50 + 20, %plus the spacer value
            PropPanelHeight = length(ProposerNames)*50 + 20,
            register(gui, spawn(fun() -> gui:start(AcceptorNames, ProposerNames,
                                                   AccPanelHeight, PropPanelHeight) end)),
            ?DBG("Up to this point it's", ok),
            gui ! {reqState, self()},
            receive
                {reqState, State} ->
                    {AccIds, PropIds} = State,
                    start_acceptors(AccIds, AccRegister, Seed),
                    AccRegister2 = lists:map(
                                     fun(X)->
                                             {X,Host}
                                     end,
                                     AccRegister),
                    ProposersProc ! {paxy_acceptors_gui, AccRegister2, PropIds}
            end
    end.

start_acceptors(AccIds, AccReg, Seed) ->
    case AccIds of
        [] ->
            ok;
        [AccId|Rest] ->
            [RegName|RegNameRest] = AccReg,
            register(RegName, acceptor:start(RegName, Seed, AccId)),
            start_acceptors(Rest, RegNameRest, Seed+1)
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
