-module(mutex).
-export([start/0,wait/0,signal/0]).
-export([free/0]).

start()  -> register(mutex, spawn(mutex, free, [])).

wait()   -> call(wait).
signal() -> call(signal).

call(Message)   ->
  mutex ! {self(), Message},
  receive
    ok -> ok
  end.

free() ->
  io:format("Mutex is now unlocked~n"),
  receive
    {Pid, wait} ->
	  io:format("Mutex locked by process ~p~n", [Pid]),
	  reply(Pid, ok),
      busy(Pid)
  end.

busy(Active) ->
  receive
	{Active, signal} ->
	  io:format("Received signal from process ~p~n", [Active]),
	  reply(Active, ok),
	  free()
  end.

reply(Pid, Message) ->
  Pid ! Message.