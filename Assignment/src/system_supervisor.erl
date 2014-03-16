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
      {one_for_all, 1, 60},
      [
        {
          factory,
          {factory, start_link, [factory, self()]},
          permanent,
          30000,
          worker,
          [factory]
        },
        {
          cc,
          {cc, start_link, [cc, self()]},
          permanent,
          30000,
          worker,
          [cc]
        }
      ]
    }
  }.