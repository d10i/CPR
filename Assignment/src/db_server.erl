-module(db_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([stop/0, write/2, read/1]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

%% API
start_link() ->
  gen_server:start_link({local, db_server}, db_server, [], []).

stop() ->
  gen_server:call(db_server, stop).
write(Key, Element) ->
  gen_server:call(db_server, {write, {Key, Element}}).
read(Key) ->
  gen_server:call(db_server, {read, Key}).

%% gen_server callbacks
init([]) ->
  io:format("Starting db_server~n"),
  {ok, db:new()}.

handle_call({write, {Key, Element}}, _From, Db) ->
  {reply, ok, db:write(Key, Element, Db)};

handle_call({read, Key}, _From, Db) ->
  {reply, db:read(Key, Db), Db};

handle_call(stop, _From, Db) ->
  db:destroy(Db),
  {stop, normal, ok, Db};

handle_call(_Request, _From, Db) ->
  {noreply, Db}.

handle_cast(_Request, Db) ->
  {noreply, Db}.

handle_info(_Info, Db) ->
  {noreply, Db}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, Db, _Extra) ->
  {ok, Db}.