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
-record(state, {requests_supervisor, db}).

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
  io:format("Starting factory~n"),
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
  cc:stop(),
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
  io:format("Terminating factory. Reason: ~p~n", [Reason]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

new_data() ->
  {[{ski, 0}, {bike, 0}, {surfboard, 0}, {skateboard, 0}], [], []}.