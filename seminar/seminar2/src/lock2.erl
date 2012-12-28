-module(lock2).
-export([init/2]).

%%--------------------------------------------------------------------
%% Function: init/2
%% Comments: Added MyLockID as a paramenter to this function
%%--------------------------------------------------------------------
init(MyLockID, Nodes) ->
    open(MyLockID, Nodes).

%%--------------------------------------------------------------------
%% Function: open/2
%% Comments: 
%%--------------------------------------------------------------------
open(MyLockID, Nodes) ->
    receive
	{take, Master} ->
	    Refs = requests(MyLockID, Nodes),
	    wait(Nodes, Master, Refs, [], MyLockID);
	{request, From, Ref, _} ->
	    From ! {ok, Ref},
	    open(MyLockID, Nodes);
	stop ->
	    ok
    end.

%%--------------------------------------------------------------------
%% Function: requests/2
%% Comments: 
%%--------------------------------------------------------------------
requests(MyLockID, Nodes) ->
    lists:map(
      fun(P) ->
	      R = make_ref(),
	      P ! {request, self(), R, MyLockID},
	      R
      end,
      Nodes).

%%--------------------------------------------------------------------
%% Function: wait/5 (1)
%% Comments: 
%%--------------------------------------------------------------------
wait(Nodes, Master, [], Waiting, MyLockID) ->
    Master ! taken,
    held(Nodes, Waiting, MyLockID);

%%--------------------------------------------------------------------
%% Function: wait/5 (2)
%% Comments: 
%%--------------------------------------------------------------------
wait(Nodes, Master, Refs, Waiting, MyLockID) ->
    receive
	{request, From, Ref, ReqID} ->
	    if
		ReqID < MyLockID ->
                    R = make_ref(),
                    Refs2 = [R|Refs],
                    From ! {request, self(), R, MyLockID},
		    From ! {ok, Ref},
                    wait(Nodes, Master, Refs2, Waiting, MyLockID);
		true ->
		    wait(Nodes, Master, Refs, [{From, Ref}|Waiting], MyLockID)
	    end;
	{ok, Ref} ->
	    Refs2 = lists:delete(Ref, Refs),
	    wait(Nodes, Master, Refs2, Waiting, MyLockID);
	release ->
	    ok(Waiting),
	    open(MyLockID, Nodes)
    end.

%%--------------------------------------------------------------------
%% Function: ok/1
%% Comments: 
%%--------------------------------------------------------------------
ok(Waiting) ->
    lists:map(
      fun({F,R}) ->
	      F ! {ok, R}
      end,
      Waiting).

%%--------------------------------------------------------------------
%% Function: held/3
%% Comments: 
%%--------------------------------------------------------------------
held(Nodes, Waiting, MyLockID) ->
    receive
	{request, From, Ref, _} ->  %% non-preemptive code
	    held(Nodes, [{From, Ref}|Waiting], MyLockID);
	release ->
	    ok(Waiting),
	    open(MyLockID, Nodes)
    end.
