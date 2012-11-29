-module(statistic).
-export([start/0]).

-ifdef(debug_statistic).
-define(DBG(X,Y,Z), io:format("[STATISTIC_DEBUG] ~w: ~s ~w~n", [X, Y, Z])).
-else.
-define(DBG(X,Y,Z), true).
-endif.

start() ->
    spawn(fun() ->
                  init()
          end).

init() ->
    ?DBG(statistic,"Starting the statistic thread",ok),
    collect(1).

collect(Round) -> %% Should only be called on the first execution
    ?DBG(statistic,"Waiting for new data",waiting),
    receive
        {statistic, Round, Total, Ok} ->
            ?DBG(statistic,"New Round started:",Round),
            Data = {Round, Total, Ok},
            collect(Round, Data, []);
        stop -> %% Nothing to return, since we have no data
            ?DBG(statistic,"Got a stop",exit),
            ok
    end.

collect(Round, Data, Table) ->
    receive
        %% New execution round
        {statistic, NewRound, Total, Ok} when NewRound > Round ->
            ?DBG(statistic,"New Round started:",NewRound),
            collect(NewRound, {NewRound, Total, Ok}, [Data|Table]);
        {statistic, Round, Total, Ok} ->
            {_, Total2, Ok2} = Data,
            collect(Round, {Round, (Total2+Total)/2, (Ok2+Ok)/2}, Table);
        {statistic, NewRound, _, _} ->
            ?DBG(statistic,"FATAL! Should not receive delayed statistics!",NewRound),
            false;
        stop ->
            ?DBG(statistic,"Received a stop! Presenting the data!",stop),
            presentData([Data|Table])
    end.

presentData(Table) ->
    io:format("Printing the Table of statistics~n~n"),
    lists:map(fun(X) ->
                      {Round, Total, Ok} = X,
                      io:format("Round ~w TOTAL: ~w, OK: ~w, -> ~w % ~n",
                                [Round, Total, Ok, 100*Ok/Total])
              end,
              Table).
