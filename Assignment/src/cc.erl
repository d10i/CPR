-module(cc).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([is_valid/3, transaction/4, add_credit_card/4]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

%% API
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

is_valid(BillingAddress, CardNumber, ExpirationDate) ->
  gen_server:call(cc, {is_valid, BillingAddress, CardNumber, ExpirationDate}).
transaction(BillingAddress, CardNumber, ExpirationDate, Price) ->
  gen_server:call(cc, {transaction, BillingAddress, CardNumber, ExpirationDate, Price}).
add_credit_card(BillingAddress, CardNumber, ExpirationDate, Balance) ->
  gen_server:call(cc, {add_credit_card, BillingAddress, CardNumber, ExpirationDate, Balance}).

%% gen_server callbacks
init(_Args) ->
  {ok, db:new()}.

handle_call({is_valid, BillingAddress, CardNumber, {ExpYear, ExpMonth}}, _Pid, Db) ->
  case get_credit_card(CardNumber, Db) of
    {ok, {BillingAddress, {ExpYear, ExpMonth}, _}} ->
      {reply, true, Db};
    {ok, _} ->
      {reply, false, Db};
    {error, not_found} ->
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

handle_call({add_credit_card, BillingAddress, CardNumber, {ExpYear, ExpMonth}, Balance}, _Pid, Db) ->
  case get_credit_card(CardNumber, Db) of
    {error, not_found} ->
      NewDb = db:write(CardNumber, {BillingAddress, {ExpYear, ExpMonth}, Balance}, Db),
      {reply, ok, NewDb};
    {ok, _} ->
      {reply, {error, credit_card_exists_already}, Db}
  end.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

get_credit_card(CardNumber, Db) ->
  case db:read(CardNumber, Db) of
    {ok, CardDetails} -> {ok, CardDetails};
    {error, _} -> {error, not_found}
  end.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.