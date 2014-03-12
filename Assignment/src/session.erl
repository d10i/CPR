-module(session).

%% API
-export([call/2]).
-export([init/1]).
-export([stop/1]).

%% Assuming a UserName can only be associated with one ReferenceId. This is because the API specs of webclient:reply
%% is reply(UserName, Message). For this reason, calling start_link with a UserName that is already associated with
%% another ReferenceId, will delete all the data for that ReferenceId and create a new one.

stop(Session) -> call(Session, stop).

init(UserName) ->
  loop(new_data(), UserName).

call(Session, Message) ->
  Session ! {request, Message}.

new_data() ->
  {[{ski, 0}, {bike, 0}, {surfboard, 0}, {skateboard, 0}], [], []}.

new_status(OldCount, NewCount) ->
  Diff = NewCount - OldCount,
  if
    Diff >= 0 -> {added, Diff};
    Diff < 0 -> {removed, -Diff}
  end.

output_action_description({Action, Diff}, NewCount, Item, UserName) ->
  FirstPart = case Action of
    added -> io_lib:format("Added ~p ~s to", [Diff, Item]);
    removed -> io_lib:format("Removed ~p ~s from", [Diff, Item])
  end,
  io:format(FirstPart ++ " the cart of ~s. Total number of ~s: ~p~n", [UserName, Item, NewCount]).

output_items_list(Cart) ->
  output_items_list(Cart, []).

output_items_list([], Strings) ->
  io:format(string:join(Strings, ", "));
output_items_list([{_, 0} | Items], Strings) ->
  output_items_list(Items, Strings);
output_items_list([{Item, Count} | Items], Strings) ->
  output_items_list(Items, lists:append(Strings, [io_lib:format("~p ~ss", [Count, atom_to_list(Item)])])).

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

loop(Data, UserName) ->
  receive
    {request, stop} ->
      ok;

    {request, {ski, N}} ->
      case Data of
        {[{ski, M}, R2, R3, R4], Ba, Cc} ->
          NewCount = max(M + N, 0),
          NewData = {[{ski, NewCount}, R2, R3, R4], Ba, Cc},
          NewStatus = new_status(M, NewCount),
          output_action_description(NewStatus, NewCount, "skis", UserName),

          webclient:reply(UserName, [NewStatus, {total, NewCount}]),
          loop(NewData, UserName)
      end ;

    {request, {bike, N}} ->
      case Data of
        {[R1, {bike, M}, R3, R4], Ba, Cc} ->
          NewCount = max(M + N, 0),
          NewData = {[R1, {bike, NewCount}, R3, R4], Ba, Cc},
          NewStatus = new_status(M, NewCount),
          output_action_description(NewStatus, NewCount, "bikes", UserName),
          webclient:reply(UserName, [NewStatus, {total, NewCount}]),
          loop(NewData, UserName)
      end ;

    {request, {surfboard, N}} ->
      case Data of
        {[R1, R2, {surfboard, M}, R4], Ba, Cc} ->
          NewCount = max(M + N, 0),
          NewData = {[R1, R2, {surfboard, NewCount}, R4], Ba, Cc},
          NewStatus = new_status(M, NewCount),
          output_action_description(NewStatus, NewCount, "surfboards", UserName),
          webclient:reply(UserName, [NewStatus, {total, NewCount}]),
          loop(NewData, UserName)
      end ;

    {request, {skateboard, N}} ->
      case Data of
        {[R1, R2, R3, {skateboard, M}], Ba, Cc} ->
          NewCount = max(M + N, 0),
          NewData = {[R1, R2, R3, {skateboard, NewCount}], Ba, Cc},
          NewStatus = new_status(M, NewCount),
          output_action_description(NewStatus, NewCount, "skateboards", UserName),
          webclient:reply(UserName, [NewStatus, {total, NewCount}]),
          loop(NewData, UserName)
      end ;

    {request, {view_cart}} ->
      case Data of
        {Cart, _, _} ->
          TotalPrice = total_price(Cart),
          case Cart of
            [{ski, N1}, {bike, N2}, {surfboard, N3}, {skateboard, N4}] ->
              io:format("The shopping cart for ~s is:~n~n", [UserName]),
              io:format("ski: ~p~nbike: ~p~nsurfboard: ~p~nskateboard: ~p~n~n", [N1, N2, N3, N4]),
              io:format("Total Price: ~s~.2f~n", [[163], TotalPrice])
          end,
          webclient:reply(UserName, {Cart, TotalPrice}),
          loop(Data, UserName)
      end ;

    {request, {billing_address, BillingAddress}} ->
      case error_items(BillingAddress) of
        [] ->
          case Data of
            {Cart, _, Cc} ->
              NewData = {Cart, BillingAddress, Cc},
              webclient:reply(UserName, ok),
              loop(NewData, UserName)
          end ;
        Items ->
          webclient:reply(UserName, {error, Items}),
          loop(Data, UserName)
      end ;

    {request, {credit_card, CardNumber, {ExpYear, ExpMonth}}} ->
      case Data of
        {Cart, BillingAddress, _} ->
          case cc:is_valid(BillingAddress, CardNumber, {ExpYear, ExpMonth}) of
            true ->
              NewData = {Cart, BillingAddress, [CardNumber, {ExpYear, ExpMonth}]},
              webclient:reply(UserName, ok),
              loop(NewData, UserName);
            false ->
              webclient:reply(UserName, {error, card_invalid}),
              loop(Data, UserName)
          end
      end ;

    {request, {buy}} ->
      case Data of
        {Cart, BillingAddress, [CardNumber, {ExpYear, ExpMonth}]} ->
          case cc:is_valid(BillingAddress, CardNumber, {ExpYear, ExpMonth}) of
            true ->
              case error_items(BillingAddress) of
                [] ->
                  TotalPrice = total_price(Cart),
                  case cc:transaction(BillingAddress, CardNumber, {ExpYear, ExpMonth}, TotalPrice) of
                    {ok, _} ->
                      NewData = new_data(),
                      io:format("~s just bought ", [UserName]),
                      output_items_list(Cart),
                      io:format(". The total price is ~s~.2f.~n", [[163], TotalPrice]),
                      webclient:reply(UserName, {ok, {Cart, TotalPrice}}),
                      loop(NewData, UserName);
                    {error, _} ->
                      webclient:reply(UserName, {error, credit_info}),
                      loop(Data, UserName)
                  end ;
                _ ->
                  webclient:reply(UserName, {error, billing_info}),
                  loop(Data, UserName)
              end ;
            false ->
              webclient:reply(UserName, {error, credit_info}),
              loop(Data, UserName)
          end ;
        {ok, _, _} ->
          webclient:reply(UserName, {error, credit_info}),
          loop(Data, UserName)
      end
  end.