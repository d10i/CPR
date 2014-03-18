-module(address_verifier).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([is_valid/1, error_items/1, stop/0]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

%% API
start_link() ->
  gen_server:start_link({local, address_verifier}, address_verifier, [], []).

is_valid(BillingAddress) ->
  gen_server:call(address_verifier, {is_valid, BillingAddress}).
error_items(BillingAddress) ->
  gen_server:call(address_verifier, {error_items, BillingAddress}).
stop() ->
  gen_server:call(address_verifier, stop).

%% gen_server callbacks
init([]) ->
  io:format("Starting address_verifier~n"),
  {ok, db:new()}.

handle_call({is_valid, BillingAddress}, _Pid, []) ->
  case error_items_internal(BillingAddress) of
    [] ->
      {reply, true, []};
    _ ->
      {reply, false, []}
  end ;

handle_call({error_items, BillingAddress}, _Pid, []) ->
  {reply, error_items_internal(BillingAddress), []};

handle_call(stop, _From, State) ->
  {stop, normal, ok, State};

handle_call(_Msg, _From, State) ->
  {noreply, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(Reason, _State) ->
  io:format("Terminating address_verifier. Reason: ~p~n", [Reason]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% TODO: this can probably be improved
error_items_internal([{address, Address}, {name, Name}, {city, City}, {country, Country}]) ->
  Err1 = verify_address(Address),
  Err2 = verify_name(Name),
  Err3 = verify_city(City),
  Err4 = verify_country(Country),
  lists:merge([Err1, Err2, Err3, Err4]);

error_items_internal(_) ->
  [address, name, city, country].

verify_address(Address) ->
  case Address of
    {Number, StreetName} when is_integer(Number) and is_list(StreetName) -> [];
    _ -> [address]
  end.

verify_name(Name) ->
  case is_list(Name) of
    true -> [];
    false -> [name]
  end.

verify_city(City) ->
  case is_list(City) of
    true -> [];
    false -> [city]
  end.

verify_country(Country) ->
  case is_list(Country) of
    true -> [];
    false -> [country]
  end.