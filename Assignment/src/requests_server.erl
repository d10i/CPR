-module(requests_server).

-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([stop/0]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

%% API
start_link(SystemSupervisor) ->
  gen_server:start_link({local, requests_server}, requests_server, [SystemSupervisor], []).

%% gen_server callbacks
-record(state, {requests_supervisor, db}).
stop() ->
  gen_server:call(requests_server, stop).

%% gen_server callbacks
init([SystemSupervisor]) ->
  io:format("Starting requests_server~n"),
  self() ! {start_requests_supervisor, SystemSupervisor},
  {ok, #state{db = db:new()}}.

handle_call({start_link, UserName}, _Pid, S = #state{db = Db}) ->
  ReferenceId = make_ref(),
  NewDb = db:write(ReferenceId, {UserName, new_data()}, Db),
  {reply, {ok, ReferenceId}, S#state{db = NewDb}};

handle_call({ReferenceId, Message}, _Pid, S = #state{requests_supervisor = RequestsSupervisor, db = Db}) ->
  {ok, {UserName, Data}} = db:read(ReferenceId, Db),
  {ok, Pid} = supervisor:start_child(RequestsSupervisor, [UserName, Data]),
  {Response, NewData} = request:call(Pid, Message),
  {reply, Response, S#state{db = db:write(ReferenceId, {UserName, NewData}, Db)}};

handle_call(stop, _From, Db) ->
  {stop, normal, ok, Db};

handle_call(_Msg, _From, State) ->
  {noreply, State}.

handle_info({start_requests_supervisor, SystemSupervisor}, S = #state{}) ->
  {ok, Pid} = supervisor:start_child(SystemSupervisor,
    {
      requests_supervisor,
      {requests_supervisor, start_link, []},
      temporary,
      10000,
      supervisor,
      [requests_supervisor, io]
    }
  ),
  link(Pid),
  {noreply, S#state{requests_supervisor = Pid}};

handle_info(Msg, State) ->
  io:format("Unknown msg: ~p~n", [Msg]),
  {noreply, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

terminate(Reason, _State) ->
  io:format("Terminating requests_server. Reason: ~p~n", [Reason]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

new_data() ->
  {[{ski, 0}, {bike, 0}, {surfboard, 0}, {skateboard, 0}], [], []}.