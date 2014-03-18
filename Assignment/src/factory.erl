-module(factory).

-behaviour(application).

% application
-export([start/2, stop/1]).
-export([start_link/1, ski/2, bike/2, surfboard/2, skateboard/2, view_cart/1, billing_address/2, credit_card/3, buy/1]).

% application callbacks
start(_Type, _Args) ->
  system_sup:start_link().

stop(_State) ->
  ok.

%% API
start_link(UserName) ->
  gen_server:call(requests_server, {start_link, UserName}).
ski(ReferenceId, N) ->
  gen_server:call(requests_server, {ReferenceId, {ski, N}}).
bike(ReferenceId, N) ->
  gen_server:call(requests_server, {ReferenceId, {bike, N}}).
surfboard(ReferenceId, N) ->
  gen_server:call(requests_server, {ReferenceId, {surfboard, N}}).
skateboard(ReferenceId, N) ->
  gen_server:call(requests_server, {ReferenceId, {skateboard, N}}).
view_cart(ReferenceId) ->
  gen_server:call(requests_server, {ReferenceId, {view_cart}}).
billing_address(ReferenceId, BillingAddress) ->
  gen_server:call(requests_server, {ReferenceId, {billing_address, BillingAddress}}).
credit_card(ReferenceId, CardNumber, ExpirationDate) ->
  gen_server:call(requests_server, {ReferenceId, {credit_card, CardNumber, ExpirationDate}}).
buy(ReferenceId) ->
  gen_server:call(requests_server, {ReferenceId, {buy}}).