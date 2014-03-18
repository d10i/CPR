-module(requests_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor
-export([init/1]).

%% API
start_link() ->
  supervisor:start_link({local, requests_sup}, requests_sup, []).

%% supervisor callbacks
init([]) ->
  io:format("Starting requests_sup~n"),
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
