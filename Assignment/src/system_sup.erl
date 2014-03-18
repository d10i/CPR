-module(system_sup).

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
  io:format("Starting system_sup~n"),
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
          requests_sup,
          {requests_sup, start_link, []},
          permanent,
          1000,
          supervisor,
          [requests_sup]
        },
        {
          util_sup,
          {util_sup, start_link, []},
          permanent,
          1000,
          supervisor,
          [util_sup]
        }
      ]
    }
  }.