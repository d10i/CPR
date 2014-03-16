-module(factory_app).

-behaviour(application).

% application
-export([start/2, stop/1]).

% application callbacks
start(_Type, _Args) ->
  system_supervisor:start_link().

stop(_State) ->
  ok.
