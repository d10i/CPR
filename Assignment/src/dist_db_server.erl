-module(dist_db_server).

-behaviour(gen_server).

%% API
-export([start_link/2]).
-export([stop/0, write/2, read/1, select/1, delete/1]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

%% API
start_link(Node1, Node2) ->
  gen_server:start_link({local, dist_db_server}, dist_db_server, [Node1, Node2], []).

-record(state, {db,
  v_clock,
  nodes}).

stop() ->
  gen_server:call(dist_db_server, stop).
write(Key, Element) ->
  io:format("~n~n~n***~n~n~nASDASD ~p~n~n~n~n***~n~n~n", [Key]),
  gen_server:call(dist_db_server, {write, Key, Element}).
read(Key) ->
  gen_server:call(dist_db_server, {read, Key}).
select(MatchSpec) ->
  gen_server:call(dist_db_server, {select, MatchSpec}).
delete(Key) ->
  gen_server:call(dist_db_server, {delete, Key}).

%% gen_server callbacks
init([Node1, Node2]) ->
  io:format("Starting dist_db_server~n"),
  {ok, #state{db = pers_db:new("data-" ++ list_to_atom(atom_to_list(node()))), v_clock = 0, nodes = [Node1, Node2]}}.

handle_call({read, Key}, _From, S = #state{db = Db}) ->
  io:format("~n~n~n***~n~n~nread ~p~n~n~n~n***~n~n~n", [Key]),
  Res = case pers_db:read(Key, Db) of
    {ok, {Element, _Stamp}} -> {ok, Element};
    X -> X
  end,
  {reply, Res, S};

handle_call({select, MatchSpec}, _From, S = #state{db = Db}) ->
  io:format("~n~n~n***~n~n~nselect ~p~n~n~n~n***~n~n~n", [MatchSpec]),
  {reply, pers_db:select(MatchSpec, Db), S};

handle_call({write, Key, Element}, _From, S = #state{v_clock = VClock, nodes = Nodes}) ->
  io:format("~n~n~n***~n~n~nwrite ~p~n~n~n~n***~n~n~n", [Key]),
  Stamp = VClock + 1,
  broadcast({dist_write, Key, Element, Stamp}, Nodes),
  {reply, ok, S#state{v_clock = Stamp}};

handle_call({dist_write, Key, Element, Stamp}, _From, S = #state{db = Db, v_clock = VClock}) ->
  io:format("~n~n~n***~n~n~ndist_write ~p~n~n~n~n***~n~n~n", [Key]),
  NewDb = pers_db:write(Key, {Element, Stamp}, Db),
  {noreply, S#state{db = NewDb, v_clock = 1 + max(VClock, Stamp)}};

handle_call({delete, Key}, _From, S = #state{v_clock = VClock, nodes = Nodes}) ->
  io:format("~n~n~n***~n~n~ndelete ~p~n~n~n~n***~n~n~n", [Key]),
  Stamp = VClock + 1,
  broadcast({dist_del, Key, Stamp}, Nodes),
  {reply, ok, S#state{v_clock = Stamp}};

handle_call({dist_del, Key, Stamp}, _From, S = #state{db = Db, v_clock = VClock}) ->
  io:format("~n~n~n***~n~n~ndist_del ~p~n~n~n~n***~n~n~n", [Key]),
  NewDb = pers_db:delete(Key, Db),
  {noreply, S#state{db = NewDb, v_clock = 1 + max(VClock, Stamp)}};

handle_call(stop, _From, Db) ->
  pers_db:destroy(Db),
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

broadcast(Msg, Nodes) ->
  io:format("~n~n~n***~n~n~nbroadcast ~p -> ~p~n~n~n~n***~n~n~n", [Msg, Nodes]),
%gen_server:multi_call(Nodes, dist_db_server, Msg),
  [gen_server:call({dist_db_server, Node}, Msg) || Node <- Nodes],
  ok.