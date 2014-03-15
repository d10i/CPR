-module(test2).

%% API
-export([start/0]).
-export([client/1]).

start() ->
  factory:start(),
  spawn(test2, client, [1]),
  spawn(test2, client, [2]),
  spawn(test2, client, [3]),
  spawn(test2, client, [4]),
  spawn(test2, client, [5]).

client(N) ->
  {ok, Ref} = factory:start_link(io_lib:format("Client #~p", [N])),
  factory:ski(Ref, 2).