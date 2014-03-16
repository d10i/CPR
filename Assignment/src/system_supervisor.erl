-module(system_supervisor).

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
  io:format("Starting system_supervisor~n"),
  {ok,
    {
      {one_for_one, 5, 60},
      [
        {
          requests_server,
          {requests_server, start_link, []},
          permanent,
          1000,
          worker,
          [requests_server]
        },
        {
          requests_supervisor,
          {requests_supervisor, start_link, []},
          permanent,
          1000,
          supervisor,
          [requests_supervisor]
        },
        {
          util_supervisor,
          {util_supervisor, start_link, []},
          permanent,
          1000,
          supervisor,
          [util_supervisor]
        }
      ]
    }
  }.