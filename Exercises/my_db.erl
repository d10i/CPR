-module(my_db).
-export([start/0, stop/0, write/2, delete/1, read/1, match/1]).
-export([init/0]).

start() ->
  register(my_db, spawn(my_db, init, [])), ok.

init() ->
  Db = db:new(),
  loop(Db).

stop() -> call(stop).
write(Key, Element) -> call({write, {Key, Element}}).
delete(Key) -> call({delete, Key}).
read(Key) -> call({read, Key}).
match(Element) -> call({match, Element}).

call(Message) ->
  my_db ! {request, self(), Message},
  receive
    {reply, Reply} -> Reply
  end.

reply(Pid, Message) ->
  Pid ! {reply, Message}.

loop(Db) ->
  receive
    {request, Pid, stop} ->
      reply(Pid, ok);
    {request, Pid, {write, {Key, Element}}} ->
      Db2 = db:write(Key, Element, Db),
      reply(Pid, ok),
      loop(Db2);
    {request, Pid, {delete, Key}} ->
      Db2 = db:delete(Key, Db),
      reply(Pid, ok),
      loop(Db2);
    {request, Pid, {read, Key}} ->
      reply(Pid, db:read(Key, Db)),
      loop(Db);
    {request, Pid, {match, Element}} ->
      reply(Pid, db:match(Element, Db)),
      loop(Db)
  end.