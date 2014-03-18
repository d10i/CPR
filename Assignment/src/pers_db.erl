-module(pers_db).
-export([new/0, new/1, write/3, read/2, select/2, delete/2, destroy/1]).

new() ->
  {ok, Db} = dets:open_file(data, []),
  Db.

new(Name) ->
  {ok, Db} = dets:open_file(Name, []),
  Db.

write(Key, Element, Db) ->
  dets:insert(Db, {Key, Element}),
  Db.

read(Key, Db) ->
  case dets:lookup(Db, Key) of
    [{Key, Element}] -> {ok, Element};
    {error, Reason} -> {error, Reason};
    _ -> {error, instance}
  end.

select(MatchSpec, Db) ->
  dets:select(Db, MatchSpec).

delete(Key, Db) ->
  dets:delete(Db, Key).

destroy(Db) ->
  dets:close(Db),
  ok.