-module(test).

%% API
-export([start/0]).

start() ->
  BillingAddress = [{address, {123, "fake St."}}, {name, "Some Name"}, {city, "London"}, {country, "UK"}],
  CardNumber = 4540123456787721,
  ExpirationDate = {16, 9},
  application:stop(factory),
  application:start(factory),
  cc:add_credit_card(BillingAddress, CardNumber, ExpirationDate, 1000.00),
  {ok, RefC} = factory:start_link("Dario"),
  factory:skateboard(RefC, 3),
  factory:surfboard(RefC, 0),
  factory:skateboard(RefC, -2),
  factory:skateboard(RefC, -3),
  factory:ski(RefC, 2),
  factory:skateboard(RefC, 4),
  factory:view_cart(RefC),
  factory:billing_address(RefC, BillingAddress),
  factory:credit_card(RefC, {4540, 1234, 5678}, {16, 9}),
  factory:credit_card(RefC, CardNumber, ExpirationDate),
  factory:buy(RefC),
  factory:view_cart(RefC).