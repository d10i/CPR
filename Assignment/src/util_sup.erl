-module(util_sup).

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% supervisor
-export([init/1]).

%% API
start_link(Node1, Node2) ->
  supervisor:start_link({local, util_sup}, util_sup, [Node1, Node2]).

%% supervisor callbacks
init([Node1, Node2]) ->
  io:format("Starting util_sup~n"),
  {ok,
    {
      {one_for_one, 5, 60},
      [
        {
          cc,
          {cc, start_link, []},
          permanent,
          1000,
          worker,
          [cc]
        },
        {
          address_verifier,
          {address_verifier, start_link, []},
          permanent,
          1000,
          worker,
          [address_verifier]
        },
        {
          dist_db_server,
          {dist_db_server, start_link, [Node1, Node2]},
          permanent,
          3000,
          worker,
          [dist_db_server]
        }
      ]
    }
  }.