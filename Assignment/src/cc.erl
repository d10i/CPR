-module(cc).

%% API
-export([is_valid/3, transaction/4, add_credit_card/4]).
-export([init/0]).
-export([start/0, stop/0]).

stop() -> call(stop).
is_valid(BillingAddress, CardNumber, ExpirationDate) ->
  call({is_valid, BillingAddress, CardNumber, ExpirationDate}).
transaction(BillingAddress, CardNumber, ExpirationDate, Price) ->
  call({transaction, BillingAddress, CardNumber, ExpirationDate, Price}).
add_credit_card(BillingAddress, CardNumber, ExpirationDate, Balance) ->
  call({add_credit_card, BillingAddress, CardNumber, ExpirationDate, Balance}).
%transaction(BillingAddress, CardNumber, {ExpYear, ExpMonth}, Price) -> {ok, trxId} | {error, invalid_card | insufficient_funds}.

start() ->
  register(cc, spawn(cc, init, [])),
  ok.

init() ->
  loop(db:new()).

call(Message) ->
  cc ! {request, self(), Message},
  receive
    {reply, Reply} -> Reply
  end.

reply(Pid, Message) ->
  Pid ! {reply, Message}.

get_credit_card(CardNumber, Db) ->
  case db:read(CardNumber, Db) of
    {ok, CardDetails} -> {ok, CardDetails};
    {error, _} -> {error, not_found}
  end.

loop(Db) ->
  receive
    {request, Pid, stop} ->
      reply(Pid, ok);

    {request, Pid, {is_valid, BillingAddress, CardNumber, {ExpYear, ExpMonth}}} ->
      case get_credit_card(CardNumber, Db) of
        {ok, {BillingAddress, {ExpYear, ExpMonth}, _}} ->
          reply(Pid, true);
        {ok, _} ->
          reply(Pid, false);
        {error, not_found} ->
          reply(Pid, false)
      end,
      loop(Db);

    {request, Pid, {transaction, BillingAddress, CardNumber, {ExpYear, ExpMonth}, Price}} ->
      case get_credit_card(CardNumber, Db) of
        {ok, {BillingAddress, {ExpYear, ExpMonth}, Balance}} ->
          if
            Price =< Balance ->
              NewDb = db:write(CardNumber, {BillingAddress, {ExpYear, ExpMonth}, Balance - Price}, Db),
              reply(Pid, {ok, make_ref()}),
              loop(NewDb);
            Price > Balance ->
              reply(Pid, {error, insufficient_funds}),
              loop(Db)
          end;
        {ok, _} ->
          reply(Pid, {error, invalid_card}),
          loop(Db);
        {error, not_found} ->
          reply(Pid, {error, invalid_card}),
          loop(Db)
      end;

    {request, Pid, {add_credit_card, BillingAddress, CardNumber, {ExpYear, ExpMonth}, Balance}} ->
      case get_credit_card(CardNumber, Db) of
        {error, not_found} ->
          NewDb = db:write(CardNumber, {BillingAddress, {ExpYear, ExpMonth}, Balance}, Db),
          reply(Pid, ok),
          loop(NewDb);
        {ok, _} ->
          reply(Pid, {error, credit_card_exists_already}),
          loop(Db)
      end
  end.