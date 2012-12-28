-module(worker).
-export([init/5]).

-define(deadlock, 5000).

init(Name, Lock, Seed, Sleep, Work) ->
    Gui = spawn(gui, init, [Name]),
    random:seed(Seed, Seed, Seed),
    Taken = worker(Name, Lock, [], Sleep, Work, Gui),
    Gui ! stop,
    terminate(Name, Taken).

worker(Name, Lock, Taken, Sleep, Work, Gui) ->
    Wait = random:uniform(Sleep),
    receive
	stop ->
	    Taken
    after Wait ->
	    T = critical(Name, Lock, Work, Gui),
	    worker(Name, Lock, [T|Taken], Sleep, Work, Gui)
    end.

critical(Name, Lock, Work, Gui) ->
    T1 = now(),
    Gui ! waiting,
    Lock ! {take, self()},
    receive
	taken ->
	    T = elapsed(T1),
	    io:format("~s: lock taken in ~w ms~n",[Name, T]),
	    Gui ! taken,
	    timer:sleep(random:uniform(Work)),
	    io:format("~s: lock released~n",[Name]),
	    Gui ! leave,
	    Lock ! release,
	    {taken, T}
    after ?deadlock ->
	    io:format("~s: giving up~n",[Name]),
	    Lock ! release,
	    Gui ! leave,
	    no
    end.

elapsed({_,S1,M1}) ->
    {_,S2,M2} = now(),
    (S2 - S1)*1000 + ((M2 - M1) div 1000).

terminate(Name, Taken) ->
    {Locks, Time, Dead} =
	lists:foldl(
	  fun(Entry,{L,T,D}) ->
		  case Entry of
		      {taken,I} ->
			  {L+1,T+I,D};
		      _ ->
			  {L,T,D+1}
		  end
	  end,
	  {0,0,0}, Taken),
    if
	Locks > 0 ->
	    Average = Time / Locks;
	true ->
	    Average = 0
    end,
    io:format("~s: ~w locks taken, average of ~w ms, ~w withdrawal situations~n",
	      [Name, Locks, Average, Dead]).

