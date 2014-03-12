-module(webclient).

%% API
-export([reply/2, associate_pid_to_username/2]).
-export([init/0]).
-export([start/0, stop/0]).

stop() -> call(stop).
reply(UserName, Message) -> call({reply, UserName, Message}).
associate_pid_to_username(UserName, Pid) -> call({associate_pid_to_username, UserName, Pid}).

start() ->
  register(webclient, spawn(webclient, init, [])),
  ok.

init() ->
  loop(db:new()).

call(Message) ->
  webclient ! {request, Message}.

get_pids_for_username(UserName, Db) ->
  case db:read(UserName, Db) of
    {ok, Pids} ->
      sets:to_list(Pids);
    {error, _} -> error
  end.

loop(Db) ->
  receive
    {request, stop} ->
      ok;

    {request, {reply, UserName, Message}} ->
      [Pid ! {reply, Message} || Pid <- get_pids_for_username(UserName, Db)],
      loop(Db);

    {request, {associate_pid_to_username, UserName, Pid}} ->
      case db:read(UserName, Db) of
        {ok, Pids} ->
          NewPids = sets:add_element(Pid, Pids),
          NewDb = db:write(UserName, NewPids, Db),
          loop(NewDb);
        {error, _} ->
          Pids = sets:new(),
          NewPids = sets:add_element(Pid, Pids),
          NewDb = db:write(UserName, NewPids, Db),
          loop(NewDb)
      end
  end.