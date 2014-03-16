-module(util_supervisor).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor
-export([init/1]).

%% API
start_link() ->
  supervisor:start_link({local, util_supervisor}, util_supervisor, []).

%% supervisor callbacks
init([]) ->
  io:format("Starting util_supervisor~n"),
  {ok,
    {
      {one_for_one, 1, 60},
      [
        {
          cc,
          {cc, start_link, []},
          permanent,
          30000,
          worker,
          [cc, io]
        },
        {
          address_verifier,
          {address_verifier, start_link, []},
          permanent,
          30000,
          worker,
          [address_verifier, io]
        }
      ]
    }
  }.