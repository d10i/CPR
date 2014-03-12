%% Copyright
-module(test).

%% API
-export([start/0]).

start() ->
  factory:start(),
  io:format("1.~n"),
  {ok, Ref} = factory:start_link("Dario"),
  io:format("2.~n"),
  factory:skateboard(Ref, 3),
  io:format("3.~n"),
  factory:surfboard(Ref, 0),
  io:format("4.~n"),
  factory:skateboard(Ref, -2),
  io:format("5.~n"),
  factory:skateboard(Ref, -3),
  io:format("6.~n"),
  factory:ski(Ref, 2),
  io:format("7.~n"),
  factory:skateboard(Ref, 4),
  io:format("8.~n"),
  factory:view_cart(Ref),
  io:format("9.~n"),
  factory:billing_address(Ref, [{address, {123, "fake St."}}, {name, "Some Name"}, {city, "London"}, {country, "UK"}]),
  io:format("10.~n"),
  factory:credit_card(Ref, {4540, 1234, 5678}, {16, 9}),
  io:format("11.~n"),
  factory:credit_card(Ref, 4540123456787721, {16, 9}),
  io:format("12.~n"),
  factory:buy(Ref),
  io:format("13.~n"),
  factory:view_cart(Ref).