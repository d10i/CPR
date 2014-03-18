-module(requests_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([stop/0]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

%% API
start_link() ->
  gen_server:start_link({local, requests_server}, requests_server, [], []).

%% gen_server callbacks
stop() ->
  gen_server:call(requests_server, stop).

%% gen_server callbacks
init([]) ->
  io:format("Starting requests_server~n"),
% Check for old session every 60 seconds
  timer:send_interval(15000, session_cleanup),
  {ok, []}.

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

handle_info(session_cleanup, State) ->
% Get all sessions that haven't been used for more than 30 minutes
  Sessions = db_server:select([{{'$1', {'$2', '$3', '$4'}},
    [{'=<', '$4', timestamp() - 15}],
    [['$1']]}]),

% Delete all sessions, one by one
  [db_server:delete(Session) || [Session] <- Sessions],

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