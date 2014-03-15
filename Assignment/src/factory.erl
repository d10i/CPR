-module(factory).

-behaviour(gen_server).

%% API
-export([start_link/2]).
-export([start_link/1, ski/2, bike/2, surfboard/2, skateboard/2, view_cart/1, billing_address/2, credit_card/3, buy/1, stop/0]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

%% API
start_link(Name, SystemSupervisor) ->
  gen_server:start_link({local, Name}, ?MODULE, [SystemSupervisor], []).

%% gen_server callbacks
-record(state, {sessionsSupervisor, sessions}).

start_link(UserName) ->
  gen_server:call(factory, {start_link, UserName}).
ski(ReferenceId, N) ->
  gen_server:call(factory, {ReferenceId, {ski, N}}).
bike(ReferenceId, N) ->
  gen_server:call(factory, {ReferenceId, {bike, N}}).
surfboard(ReferenceId, N) ->
  gen_server:call(factory, {ReferenceId, {surfboard, N}}).
skateboard(ReferenceId, N) ->
  gen_server:call(factory, {ReferenceId, {skateboard, N}}).
view_cart(ReferenceId) ->
  gen_server:call(factory, {ReferenceId, {view_cart}}).
billing_address(ReferenceId, BillingAddress) ->
  gen_server:call(factory, {ReferenceId, {billing_address, BillingAddress}}).
credit_card(ReferenceId, CardNumber, ExpirationDate) ->
  gen_server:call(factory, {ReferenceId, {credit_card, CardNumber, ExpirationDate}}).
buy(ReferenceId) ->
  gen_server:call(factory, {ReferenceId, {buy}}).
stop() ->
  gen_server:call(factory, stop).

%% gen_server callbacks
init([SystemSupervisor]) ->
  self() ! {start_sessions_supervisor, SystemSupervisor},
  {ok, #state{sessions = db:new()}}.

handle_call({start_link, UserName}, _Pid, S = #state{sessionsSupervisor = SessionsSupervisor, sessions = Sessions}) ->
  ReferenceId = make_ref(),
  {ok, Pid} = supervisor:start_child(SessionsSupervisor,
    {
      session,
      {session, start_link, [UserName]},
      temporary,
      10000,
      worker,
      [session]
    }
  ),
%Session = erlang:monitor(process, Pid),
  {reply, {ok, ReferenceId}, S#state{sessions = db:write(ReferenceId, Pid, Sessions)}};

handle_call({ReferenceId, Message}, _Pid, S = #state{sessions = Sessions}) ->
  Session = get_session(ReferenceId, Sessions),
  {reply, session:call(Session, Message), S};

handle_call(stop, _From, Db) ->
  cc:stop(),
% TODO: stop all sessions?
  {stop, normal, ok, Db};

handle_call(_Msg, _From, State) ->
  {noreply, State}.

handle_info({start_sessions_supervisor, SystemSupervisor}, S = #state{}) ->
  {ok, Pid} = supervisor:start_child(SystemSupervisor,
    {
      session_supervisor,
      {session_supervisor, start_link, []},
      temporary,
      10000,
      supervisor,
      [session_supervisor]
    }
  ),
  link(Pid),
  {noreply, S#state{sessionsSupervisor = Pid}};

handle_info(Msg, State) ->
  io:format("Unknown msg: ~p~n", [Msg]),
  {noreply, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

get_session(ReferenceId, Db) ->
  case db:read(ReferenceId, Db) of
    {ok, Session} -> Session;
    {error, _} -> error
  end.