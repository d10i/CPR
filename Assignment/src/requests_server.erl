-module(requests_server).

-behaviour(gen_server).

%% API
-export([start_link/2]).
-export([start_link/1, ski/2, bike/2, surfboard/2, skateboard/2, view_cart/1, billing_address/2, credit_card/3, buy/1]).
-export([stop/0]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

%% API
start_link(Node1, Node2) ->
  gen_server:start_link(requests_server, [Node1, Node2], []).

start_link(UserName) ->
  gen_server:call({global, requests_server}, {start_link, UserName}).
ski(ReferenceId, N) ->
  gen_server:call({global, requests_server}, {ReferenceId, {ski, N}}).
bike(ReferenceId, N) ->
  gen_server:call({global, requests_server}, {ReferenceId, {bike, N}}).
surfboard(ReferenceId, N) ->
  gen_server:call({global, requests_server}, {ReferenceId, {surfboard, N}}).
skateboard(ReferenceId, N) ->
  gen_server:call({global, requests_server}, {ReferenceId, {skateboard, N}}).
view_cart(ReferenceId) ->
  gen_server:call({global, requests_server}, {ReferenceId, {view_cart}}).
billing_address(ReferenceId, BillingAddress) ->
  gen_server:call({global, requests_server}, {ReferenceId, {billing_address, BillingAddress}}).
credit_card(ReferenceId, CardNumber, ExpirationDate) ->
  gen_server:call({global, requests_server}, {ReferenceId, {credit_card, CardNumber, ExpirationDate}}).
buy(ReferenceId) ->
  gen_server:call({global, requests_server}, {ReferenceId, {buy}}).

stop() ->
  gen_server:call({global, requests_server}, stop).

%% gen_server callbacks
init([Node1, Node2]) ->
  io:format("Starting requests_server~n"),
% Check for old session every 60 seconds

  OtherNode = other_node(Node1, Node2),
  io:format("Other node: ~p~n", [OtherNode]),

% Connects to other node
  {factory, OtherNode} ! ping,

% It takes a bit of time to connect to the other node, let's wait 2 seconds just to be sure
% Otherwise if it connects after the global name is registered, it might unregister the name
  timer:sleep(2000),

  case global:whereis_name(requests_server) of
    undefined ->
% This has been registered as the primary node
      global:register_name(requests_server, self()),
      io:format("Node ~p registered as primary~n", [node()]);
    _ ->
% This is the backup node
      io:format("Node ~p registered as backup~n", [node()]),
      erlang:monitor_node(OtherNode, true)
  end,


%timer:send_interval(15000, session_cleanup),
  {ok, [Node1, Node2]}.

handle_call({start_link, UserName}, _Pid, State) ->
  ReferenceId = make_ref(),
  {ok, Pid} = supervisor:start_child(requests_sup, [ReferenceId]),
  Response = request:call(Pid, {start_link, UserName}),
  {reply, Response, State};

handle_call({ReferenceId, Message}, _Pid, State) ->
  {ok, Pid} = supervisor:start_child(requests_sup, [ReferenceId]),
  Response = request:call(Pid, Message),
  {reply, Response, State};

handle_call(stop, _From, State) ->
  {stop, normal, ok, State};

handle_call(_Msg, _From, State) ->
  {noreply, State}.

handle_info({nodedown, Node}, S) ->
  io:format("Node ~p is monitoring and received nodedown from ~p~n", [node(), Node]),
  io:format("Now becoming primary~n"),
  global:register_name(requests_server, self()),
  {noreply, S};

handle_info(session_cleanup, State) ->
% Get all sessions that haven't been used for more than 30 minutes
  Sessions = dist_db_server:select([{{'$1', {{'$2', '$3', '$4'}, '$5'}},
    [{'=<', '$4', timestamp() - 15}],
    [['$1']]}]),

% Delete all sessions, one by one
  [dist_db_server:delete(Session) || [Session] <- Sessions],

  {noreply, State};

handle_info(_Msg, State) ->
  {noreply, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

terminate(Reason, _State) ->
  io:format("Terminating requests_server. Reason: ~p~n", [Reason]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

timestamp() ->
  {MegaSecs, Secs, _MicroSecs} = os:timestamp(),
  1000000 * MegaSecs + Secs.

other_node(Node1, Node2) ->
  case node() of
    Node1 -> Node2;
    Node2 -> Node1
  end.