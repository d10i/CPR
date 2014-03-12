-module(cc).

%% API
-export([is_valid/3,transaction/4]).

is_valid(BillingAddress, CardNumber, {ExpYear, ExpMonth}) ->
  is_integer(CardNumber) and is_integer(ExpYear) and is_integer(ExpMonth).
transaction(BillingAddress, CardNumber, {ExpYear, ExpMonth}, Price) -> {ok, make_ref()}.
%transaction(BillingAddress, CardNumber, {ExpYear, ExpMonth}, Price) -> {ok, trxId} | {error, invalid_card | insufficient_funds}.