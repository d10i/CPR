-module(reliable_mutex).
-export([start/0,wait/0,signal/0]).
-export([init/0]).

start()  -> register(reliable_mutex, spawn(reliable_mutex, init, [])).

wait()   -> call(wait).
signal() -> call(signal).

call(Message)   ->
  reliable_mutex ! {self(), Message},
  receive
    ok -> ok
  end.

init() ->
  process_flag(trap_exit, true),
  free().

free() ->
  io:format("Mutex is now unlocked~n"),
  receive
    {Pid, wait} ->
	  case link(Pid) of
	    {'EXIT', _} ->
          io:format("Process terminated while waiting for signal ~p~n", [Pid]),
		  free();
		true ->
          io:format("Mutex locked by process ~p~n", [Pid]),
          reply(Pid, ok),
          busy(Pid)
      end
  end.

busy(Active) ->
  receive
	{'EXIT', Active, _} ->
	  io:format("Active process ~p exited~n", [Active]),
	  free();
	{Active, signal} ->
	  io:format("Received signal from process ~p~n", [Active]),
	  reply(Active, ok),
	  free()
  end.

reply(Pid, Message) ->
  Pid ! Message.