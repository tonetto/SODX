-module(lock1).
-export([init/2]).

init(_, Nodes) ->
    open(Nodes).

open(Nodes) ->
    receive
	{take, Master} ->
	    Refs = requests(Nodes),
	    wait(Nodes, Master, Refs, []);
	{request, From, Ref} ->
	    From ! {ok, Ref},
	    open(Nodes);
	stop ->
	    ok
    end.

requests(Nodes) ->
    lists:map(
      fun(P) ->
	      R = make_ref(),
	      P ! {request, self(), R},
	      R
      end,
      Nodes).

wait(Nodes, Master, [], Waiting) ->
    Master ! taken,
    held(Nodes, Waiting);
wait(Nodes, Master, Refs, Waiting) ->
    receive
	{request, From, Ref} ->
	    wait(Nodes, Master, Refs, [{From, Ref}|Waiting]);
	{ok, Ref} ->
	    Refs2 = lists:delete(Ref, Refs),
	    wait(Nodes, Master, Refs2, Waiting);
	release ->
	    ok(Waiting),
	    open(Nodes)
    end.

ok(Waiting) ->
    lists:map(
      fun({F,R}) ->
	      F ! {ok, R}
      end,
      Waiting).

held(Nodes, Waiting) ->
    receive
	{request, From, Ref} ->
	    held(Nodes, [{From, Ref}|Waiting]);
	release ->
	    ok(Waiting),
	    open(Nodes)
    end.
