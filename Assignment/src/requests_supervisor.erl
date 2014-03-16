-module(requests_supervisor).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor
-export([init/1]).

%% API
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor callbacks
init([]) ->
  io:format("Starting requests_supervisor~n"),
  {
    ok,
    {
      {simple_one_for_one, 5, 3600},
      [
        {
          ppool_worker,
          {request, start_link, []},
          temporary,
          5000,
          worker,
          [request, webclient]
        }
      ]
    }
  }.
