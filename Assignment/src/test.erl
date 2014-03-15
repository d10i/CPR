-module(test).

%% API
-export([start/0]).

start() ->
  BillingAddress = [{address, {123, "fake St."}}, {name, "Some Name"}, {city, "London"}, {country, "UK"}],
  CardNumber = 4540123456787721,
  ExpirationDate = {16, 9},
  factory:start_link(),
  cc:add_credit_card(BillingAddress, CardNumber, ExpirationDate, 1000.00),
  {ok, Ref} = factory:start_link("Dario"),
  factory:skateboard(Ref, 3),
  factory:surfboard(Ref, 0),
  factory:skateboard(Ref, -2),
  factory:skateboard(Ref, -3),
  factory:ski(Ref, 2),
  factory:skateboard(Ref, 4),
  factory:view_cart(Ref),
  factory:billing_address(Ref, BillingAddress),
  factory:credit_card(Ref, {4540, 1234, 5678}, {16, 9}),
  factory:credit_card(Ref, CardNumber, ExpirationDate),
  factory:buy(Ref),
  factory:view_cart(Ref).