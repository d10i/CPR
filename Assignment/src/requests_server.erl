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
  {ok, []}.

handle_call({start_link, UserName}, _Pid, State) ->
  ReferenceId = make_ref(),
  db_server:write(ReferenceId, {UserName, new_data()}),
  {reply, {ok, ReferenceId}, State};

handle_call({ReferenceId, Message}, _Pid, State) ->
  {ok, {UserName, Data}} = db_server:read(ReferenceId),
  {ok, Pid} = supervisor:start_child(requests_supervisor, [UserName, Data]),
  {Response, NewData} = request:call(Pid, Message),
  db_server:write(ReferenceId, {UserName, NewData}),
  {reply, Response, State};

handle_call(stop, _From, State) ->
  {stop, normal, ok, State};

handle_call(_Msg, _From, State) ->
  {noreply, State}.

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