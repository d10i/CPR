-module(factory).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([start_link/1, ski/2, bike/2, surfboard/2, skateboard/2, view_cart/1, billing_address/2, credit_card/3, buy/1]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

%% API
start_link() ->
  cc:start_link(),
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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

%% gen_server callbacks
init(_Args) ->
  {ok, db:new()}.

handle_call({start_link, UserName}, _Pid, Db) ->
  ReferenceId = make_ref(),
  {ok, Session} = session:start(UserName),
  NewDb = db:write(ReferenceId, Session, Db),
  {reply, {ok, ReferenceId}, NewDb};

handle_call({ReferenceId, Message}, _Pid, Db) ->
  Session = get_session(ReferenceId, Db),
  {reply, session:call(Session, Message), Db}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
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