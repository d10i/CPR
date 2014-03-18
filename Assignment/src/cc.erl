-module(cc).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([is_valid/3, transaction/4, stop/0]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

%% API
start_link() ->
  gen_server:start_link({local, cc}, cc, [], []).

is_valid(BillingAddress, CardNumber, ExpirationDate) ->
  gen_server:call(cc, {is_valid, BillingAddress, CardNumber, ExpirationDate}).
transaction(BillingAddress, CardNumber, ExpirationDate, Price) ->
  gen_server:call(cc, {transaction, BillingAddress, CardNumber, ExpirationDate, Price}).
stop() ->
  gen_server:call(cc, stop),
  application:start(factory).

%% gen_server callbacks
init([]) ->
  io:format("Starting cc~n"),
  Db = db:new(),
  BillingAddress = [{address, {123, "fake St."}}, {name, "Some Name"}, {city, "London"}, {country, "UK"}],
  CardNumber = 4540123456787721,
  ExpirationDate = {16, 9},
  Db2 = db:write(CardNumber, {BillingAddress, ExpirationDate, 1000.00}, Db),
  {ok, Db2}.

handle_call({is_valid, BillingAddress, CardNumber, ExpirationDate}, _Pid, Db) ->
  case ExpirationDate of
    {ExpYear, ExpMonth} ->
      case get_credit_card(CardNumber, Db) of
        {ok, {BillingAddress, {ExpYear, ExpMonth}, _}} ->
          {reply, true, Db};
        {ok, _} ->
          {reply, false, Db};
        {error, not_found} ->
          {reply, false, Db}
      end;
    _ ->
      {reply, false, Db}
  end;

handle_call({transaction, BillingAddress, CardNumber, {ExpYear, ExpMonth}, Price}, _Pid, Db) ->
  case get_credit_card(CardNumber, Db) of
    {ok, {BillingAddress, {ExpYear, ExpMonth}, Balance}} ->
      if
        Price =< Balance ->
          NewDb = db:write(CardNumber, {BillingAddress, {ExpYear, ExpMonth}, Balance - Price}, Db),
          {reply, {ok, make_ref()}, NewDb};
        Price > Balance ->
          {reply, {error, insufficient_funds}, Db}
      end;
    {ok, _} ->
      {reply, {error, invalid_card}, Db};
    {error, not_found} ->
      {reply, {error, invalid_card}, Db}
  end;

handle_call(stop, _From, State) ->
  {stop, normal, ok, State};

handle_call(_Msg, _From, State) ->
  {noreply, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(Reason, _State) ->
  io:format("Terminating cc. Reason: ~p~n", [Reason]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

get_credit_card(CardNumber, Db) ->
  timer:sleep(2000),
  case db:read(CardNumber, Db) of
    {ok, CardDetails} -> {ok, CardDetails};
    {error, _} -> {error, not_found}
  end.