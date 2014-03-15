-module(session).

-behaviour(gen_server).

%% API
-export([start/1]).
-export([call/2]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

%% API
start(UserName) ->
  gen_server:start(?MODULE, [UserName], []).

call(Session, Message) ->
  gen_server:call(Session, Message).

%% gen_server callbacks
init([UserName]) ->
  {ok, {new_data(), UserName}}.


handle_call({ski, N}, _From, {Data, UserName}) ->
  case Data of
    {[{ski, M}, R2, R3, R4], Ba, Cc} ->
      NewCount = max(M + N, 0),
      NewData = {[{ski, NewCount}, R2, R3, R4], Ba, Cc},
      NewStatus = new_status(M, NewCount),
      reply({ski, [NewStatus, {total, NewCount}]}, {NewData, UserName})
  end ;

handle_call({bike, N}, _From, {Data, UserName}) ->
  case Data of
    {[R1, {bike, M}, R3, R4], Ba, Cc} ->
      NewCount = max(M + N, 0),
      NewData = {[R1, {bike, NewCount}, R3, R4], Ba, Cc},
      NewStatus = new_status(M, NewCount),
      reply({bike, [NewStatus, {total, NewCount}]}, {NewData, UserName})
  end ;

handle_call({surfboard, N}, _From, {Data, UserName}) ->
  case Data of
    {[R1, R2, {surfboard, M}, R4], Ba, Cc} ->
      NewCount = max(M + N, 0),
      NewData = {[R1, R2, {surfboard, NewCount}, R4], Ba, Cc},
      NewStatus = new_status(M, NewCount),
      reply({surfboard, [NewStatus, {total, NewCount}]}, {NewData, UserName})
  end ;

handle_call({skateboard, N}, _From, {Data, UserName}) ->
  case Data of
    {[R1, R2, R3, {skateboard, M}], Ba, Cc} ->
      NewCount = max(M + N, 0),
      NewData = {[R1, R2, R3, {skateboard, NewCount}], Ba, Cc},
      NewStatus = new_status(M, NewCount),
      reply({skateboard, [NewStatus, {total, NewCount}]}, {NewData, UserName})
  end ;

handle_call({view_cart}, _From, {Data, UserName}) ->
  case Data of
    {Cart, _, _} ->
      reply({view_cart, {Cart, total_price(Cart)}}, {Data, UserName})
  end ;

handle_call({billing_address, BillingAddress}, _From, {Data, UserName}) ->
  case error_items(BillingAddress) of
    [] ->
      case Data of
        {Cart, _, Cc} ->
          NewData = {Cart, BillingAddress, Cc},
          reply({billing_address, ok}, {NewData, UserName})
      end ;
    Items ->
      reply({billing_address, {error, Items}}, {Data, UserName})
  end ;

handle_call({credit_card, CardNumber, {ExpYear, ExpMonth}}, _From, {Data, UserName}) ->
  case Data of
    {Cart, BillingAddress, _} ->
      case cc:is_valid(BillingAddress, CardNumber, {ExpYear, ExpMonth}) of
        true ->
          NewData = {Cart, BillingAddress, [CardNumber, {ExpYear, ExpMonth}]},
          reply({credit_card, ok}, {NewData, UserName});
        false ->
          reply({credit_card, {error, card_invalid}}, {Data, UserName})
      end
  end ;

handle_call({buy}, _From, {Data, UserName}) ->
  case Data of
    {Cart, BillingAddress, Cc} ->
      case Cc of
        [CardNumber, {ExpYear, ExpMonth}] ->
          case cc:is_valid(BillingAddress, CardNumber, {ExpYear, ExpMonth}) of
            true ->
              case error_items(BillingAddress) of
                [] ->
                  TotalPrice = total_price(Cart),
                  case cc:transaction(BillingAddress, CardNumber, {ExpYear, ExpMonth}, TotalPrice) of
                    {ok, _} ->
                      NewData = new_data(),
                      reply({buy, {ok, {Cart, TotalPrice}}}, {NewData, UserName});
                    {error, _} ->
                      reply({buy, {error, credit_info}}, {Data, UserName})
                  end ;
                _ ->
                  reply({buy, {error, billing_info}}, {Data, UserName})
              end ;
            false ->
              reply({buy, {error, credit_info}}, {Data, UserName})
          end ;
        [] ->
          reply({buy, {error, credit_info}}, {Data, UserName})
      end
  end.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

new_data() ->
  {[{ski, 0}, {bike, 0}, {surfboard, 0}, {skateboard, 0}], [], []}.

reply({Action, Reply}, {Data, UserName}) ->
  webclient:reply(UserName, {Action, Reply}),
  {reply, Reply, {Data, UserName}}.

new_status(OldCount, NewCount) ->
  Diff = NewCount - OldCount,
  if
    Diff >= 0 -> {added, Diff};
    Diff < 0 -> {removed, -Diff}
  end.


total_price([{ski, SkiCount}, {bike, BikeCount}, {surfboard, SkateboardCount}, {skateboard, SurfboardCount}]) ->
  price(ski, SkiCount) +
    price(bike, BikeCount) +
    price(surfboard, SkateboardCount) +
    price(skateboard, SurfboardCount).

price(ski, Count) -> 150.0 * Count;
price(bike, Count) -> 175.0 * Count;
price(surfboard, Count) -> 175.0 * Count;
price(skateboard, Count) -> 50.0 * Count.

error_items([{address, Address}, {name, Name}, {city, City}, {country, Country}]) ->
  Err1 = case Address of
    {Number, StreetName} when is_integer(Number) and is_list(StreetName) -> [];
    _ -> [address]
  end,
  Err2 = case is_list(Name) of
    true -> [];
    false -> [name]
  end,
  Err3 = case is_list(City) of
    true -> [];
    false -> [city]
  end,
  Err4 = case is_list(Country) of
    true -> [];
    false -> [country]
  end,
  lists:merge([Err1, Err2, Err3, Err4]).