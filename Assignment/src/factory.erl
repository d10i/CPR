-module(factory).

-behaviour(application).

% application
-export([start/2, stop/1]).
-export([start_link/1, ski/2, bike/2, surfboard/2, skateboard/2, view_cart/1, billing_address/2, credit_card/3, buy/1]).

% application callbacks
start(normal, [Node1, Node2]) ->
  system_sup:start_link(Node1, Node2).

stop(_State) ->
  ok.

%% API
start_link(UserName) ->
  requests_server:start_link(UserName).
ski(ReferenceId, N) ->
  requests_server:ski(ReferenceId, N).
bike(ReferenceId, N) ->
  requests_server:bike(ReferenceId, N).
surfboard(ReferenceId, N) ->
  requests_server:surfboard(ReferenceId, N).
skateboard(ReferenceId, N) ->
  requests_server:skateboard(ReferenceId, N).
view_cart(ReferenceId) ->
  requests_server:view_cart(ReferenceId).
billing_address(ReferenceId, BillingAddress) ->
  requests_server:billing_address(ReferenceId, BillingAddress).
credit_card(ReferenceId, CardNumber, ExpirationDate) ->
  requests_server:credit_card(ReferenceId, CardNumber, ExpirationDate).
buy(ReferenceId) ->
  requests_server:buy(ReferenceId).