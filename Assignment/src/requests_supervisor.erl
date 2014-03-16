-module(requests_supervisor).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor
-export([init/1]).

%% API
start_link() ->
  supervisor:start_link({local, requests_supervisor}, requests_supervisor, []).

%% supervisor callbacks
init([]) ->
  io:format("Starting requests_supervisor~n"),
  {
    ok,
    {
      {simple_one_for_one, 10, 10},
      [
        {
          request,
          {request, start_link, []},
          temporary,
          1000,
          worker,
          [request]
        }
      ]
    }
  }.
