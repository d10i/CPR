-module(db).
-export([new/0, write/3, read/2, destroy/1]).
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

read(Key, [{Key, Element} | _Db]) ->
  {ok, Element};
read(Key, [_Tuple | Db]) ->
  read(Key, Db);
read(_Key, []) ->
  {error, instance}.

destroy(_Db) ->
  ok.