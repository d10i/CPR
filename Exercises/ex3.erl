-module(ex3).
-export([start/0,stop/0,print/1]).
-export([init/0]).

start() ->
  register(server, spawn(ex3, init, [])).

stop() ->
  call(stop).

print(Message) ->
  call({print, Message})
  
loop(Pid) ->
  receive
    {print, Message} ->
	  io:format("~p ", [Message]),
	  reply(Pid, ok),
	  loop(Pid);
    {stop} ->
      reply(Pid, ok)
  end.
  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
start(ProcNum, Count) ->
  spawn(?MODULE, init, [ProcNum, Count]).

init(ProcNum, Count) ->
  Pid = spawn(?MODULE, start_proc, [ProcNum-1, self()]),
  send_msg(Count, Pid, hello).

send_msg(0, Pid, _) ->
  Pid ! stop,
  receive stop -> stop end;
send_msg(Count, Pid, Msg) ->
  Pid ! Msg,
  receive Msg -> ok end,
  send_msg(Count-1, Pid, Msg).

start_proc(1, MasterPid) ->
  MasterPid ! ok,
  loop(MasterPid);
start_proc(ProcNum, MasterPid) ->
  Pid = spawn(?MODULE, start_proc, [ProcNum-1, MasterPid]),
  loop(Pid).

loop(Pid) ->
  receive
    stop ->
	  Pid ! stop;
	Msg ->
	  Pid ! Msg,
	  loop(Pid)
  end.