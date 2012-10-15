-module(lock3).
-export([init/2]).

%%--------------------------------------------------------------------
%% Function: init/2
%% Comments: Added MyLockID as a paramenter to this function
%%--------------------------------------------------------------------
init(MyLockID, Nodes) ->
    LamportClock = 0,
    open(MyLockID, Nodes, LamportClock).

%%--------------------------------------------------------------------
%% Function: open/3
%% Comments: 
%%--------------------------------------------------------------------
open(MyLockID, Nodes, LamportClock) ->
    receive
	{take, Master} ->
            TakeTime = LamportClock,
	    Refs = requests(MyLockID, Nodes, TakeTime),
	    wait(Nodes, Master, Refs, [], MyLockID, TakeTime, LamportClock);
	{request, From, Ref, _, ExtClock} ->
            LamportClock2 = updateClock(LamportClock, ExtClock),
	    From ! {ok, Ref, LamportClock2},
	    open(MyLockID, Nodes, LamportClock2);
	stop ->
	    ok
    end.

%%--------------------------------------------------------------------
%% Function: requests/3
%% Comments: 
%%--------------------------------------------------------------------
requests(MyLockID, Nodes, TakeTime) ->
    lists:map(
      fun(P) ->
	      R = make_ref(),
	      P ! {request, self(), R, MyLockID, TakeTime},
	      {P,R}
      end,
      Nodes).

%%--------------------------------------------------------------------
%% Function: wait/7 (1)
%% Comments: 
%%--------------------------------------------------------------------
wait(Nodes, Master, [], Waiting, MyLockID, _, LamportClock) ->
    Master ! taken,
    held(Nodes, Waiting, MyLockID, LamportClock);

%%--------------------------------------------------------------------
%% Function: wait/7 (2)
%% Comments: 
%%--------------------------------------------------------------------
wait(Nodes, Master, Refs, Waiting, MyLockID, TakeTime, LamportClock) ->
    receive
	{request, From, Ref, ReqID, ExtClock} ->
            LamportClock2 = updateClock(LamportClock, ExtClock),
            if
                TakeTime < ExtClock ->
                    wait(Nodes, Master, Refs, [{From, Ref}|Waiting], MyLockID, TakeTime, LamportClock2);
                TakeTime > ExtClock ->
                    Refs2 = resendRequest(From, Refs, MyLockID, LamportClock2),
                    From ! {ok, Ref, LamportClock2},
                    wait(Nodes, Master, Refs2, Waiting, MyLockID, TakeTime, LamportClock2);
                TakeTime == ExtClock ->
                    if
                        ReqID < MyLockID ->
                            Refs2 = resendRequest(From, Refs, MyLockID, LamportClock2),
                            From ! {ok, Ref, LamportClock2},
                            wait(Nodes, Master, Refs2, Waiting, MyLockID, TakeTime, LamportClock2);
                        true ->
                            wait(Nodes, Master, Refs, [{From, Ref}|Waiting], MyLockID, TakeTime, LamportClock2)
                    end
            end;
	{ok, Ref, ExtClock} ->
            LamportClock2 = updateClock(LamportClock, ExtClock),
	    Refs2 = lists:keydelete(Ref, 2, Refs),
	    wait(Nodes, Master, Refs2, Waiting, MyLockID, TakeTime, LamportClock2);
	release ->
	    ok(Waiting, LamportClock),
	    open(MyLockID, Nodes, LamportClock)
    end.

%%--------------------------------------------------------------------
%% Function: ok/2
%% Comments: 
%%--------------------------------------------------------------------
ok(Waiting, LamportClock) ->
    lists:map(
      fun({F,R}) ->
	      F ! {ok, R, LamportClock}
      end,
      Waiting).

%%--------------------------------------------------------------------
%% Function: held/4
%% Comments: 
%%--------------------------------------------------------------------
held(Nodes, Waiting, MyLockID, LamportClock) ->
    receive
	{request, From, Ref, _, ExtClock} ->
            LamportClock2 = updateClock(LamportClock, ExtClock),
	    held(Nodes, [{From, Ref}|Waiting], MyLockID, LamportClock2);
	release ->
	    ok(Waiting, LamportClock),
	    open(MyLockID, Nodes, LamportClock)
    end.

%%--------------------------------------------------------------------
%% Function: resendRequest/5
%% Comments: 
%%--------------------------------------------------------------------
resendRequest(From, Refs, MyLockID, LamportClock) ->
    IsPidPresent  = lists:keymember(From,1,Refs),
    IsNamePresent = lists:keymember(process_info(From),1,Refs),
    if
        IsPidPresent or IsNamePresent->
            R = make_ref(),
            Refs2 = [R|Refs],
            From ! {request, self(), R, MyLockID, LamportClock},
            Refs2;
        true ->
            Refs
    end.

%%--------------------------------------------------------------------
%% Function: updateClock/2
%% Comments: 
%%--------------------------------------------------------------------
updateClock(MyClock, ExtClock) ->
    if
        MyClock >= ExtClock ->
            NewClock = MyClock + 1;
        MyClock < ExtClock ->
            NewClock = ExtClock + 1
    end,
    NewClock.
