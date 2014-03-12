-module(db).
-export([new/0, write/3, delete/2, read/2, match/2, destroy/1]).
-export_type([db/0]).
-type db() :: list().

new() ->
  [].

write(Key, Element, []) ->
  [{Key, Element}];
write(Key, Element, [{Key, _} | Db]) ->
  [{Key, Element} | Db];
write(Key, Element, [Current | Db]) ->
  [Current | write(Key, Element, Db)].

delete(Key, [{Key, _Element} | Db]) ->
  Db;
delete(Key, [Tuple | Db]) ->
  [Tuple | delete(Key, Db)];
delete(_Key, []) ->
  [].

read(Key, [{Key, Element} | _Db]) ->
  {ok, Element};
read(Key, [_Tuple | Db]) ->
  read(Key, Db);
read(_Key, []) ->
  {error, instance}.

match(Element, [{Key, Element} | Db]) ->
  [Key | match(Element, Db)];
match(Element, [_Tuple | Db]) ->
  match(Element, Db);
match(_Key, []) ->
  [].

destroy(_Db) ->
  ok.