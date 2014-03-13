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
  Session ! {request, self(), Message},
  receive
    {reply, UserName, {Action, Reply}} ->
      webclient:reply(UserName, {Action, Reply}),
      Reply;
    {reply, Reply} ->
      Reply
  end.

reply(Pid, Username, Message) ->
  Pid ! {reply, Username, Message}.

reply(Pid, Reply) ->
  Pid ! {reply, Reply}.

new_data() ->
  {[{ski, 0}, {bike, 0}, {surfboard, 0}, {skateboard, 0}], [], []}.

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

loop(Data, UserName) ->
  receive
    {request, Pid, stop} ->
      reply(Pid, ok);

    {request, Pid, {ski, N}} ->
      case Data of
        {[{ski, M}, R2, R3, R4], Ba, Cc} ->
          NewCount = max(M + N, 0),
          NewData = {[{ski, NewCount}, R2, R3, R4], Ba, Cc},
          NewStatus = new_status(M, NewCount),
          reply(Pid, UserName, {ski, [NewStatus, {total, NewCount}]}),
          loop(NewData, UserName)
      end ;

    {request, Pid, {bike, N}} ->
      case Data of
        {[R1, {bike, M}, R3, R4], Ba, Cc} ->
          NewCount = max(M + N, 0),
          NewData = {[R1, {bike, NewCount}, R3, R4], Ba, Cc},
          NewStatus = new_status(M, NewCount),
          reply(Pid, UserName, {bike, [NewStatus, {total, NewCount}]}),
          loop(NewData, UserName)
      end ;

    {request, Pid, {surfboard, N}} ->
      case Data of
        {[R1, R2, {surfboard, M}, R4], Ba, Cc} ->
          NewCount = max(M + N, 0),
          NewData = {[R1, R2, {surfboard, NewCount}, R4], Ba, Cc},
          NewStatus = new_status(M, NewCount),
          reply(Pid, UserName, {surfboard, [NewStatus, {total, NewCount}]}),
          loop(NewData, UserName)
      end ;

    {request, Pid, {skateboard, N}} ->
      case Data of
        {[R1, R2, R3, {skateboard, M}], Ba, Cc} ->
          NewCount = max(M + N, 0),
          NewData = {[R1, R2, R3, {skateboard, NewCount}], Ba, Cc},
          NewStatus = new_status(M, NewCount),
          reply(Pid, UserName, {skateboard, [NewStatus, {total, NewCount}]}),
          loop(NewData, UserName)
      end ;

    {request, Pid, {view_cart}} ->
      case Data of
        {Cart, _, _} ->
          reply(Pid, UserName, {view_cart, {Cart, total_price(Cart)}}),
          loop(Data, UserName)
      end ;

    {request, Pid, {billing_address, BillingAddress}} ->
      case error_items(BillingAddress) of
        [] ->
          case Data of
            {Cart, _, Cc} ->
              NewData = {Cart, BillingAddress, Cc},
              reply(Pid, UserName, {billing_address, ok}),
              loop(NewData, UserName)
          end ;
        Items ->
          reply(Pid, UserName, {billing_address, {error, Items}}),
          loop(Data, UserName)
      end ;

    {request, Pid, {credit_card, CardNumber, {ExpYear, ExpMonth}}} ->
      case Data of
        {Cart, BillingAddress, _} ->
          case cc:is_valid(BillingAddress, CardNumber, {ExpYear, ExpMonth}) of
            true ->
              NewData = {Cart, BillingAddress, [CardNumber, {ExpYear, ExpMonth}]},
              reply(Pid, UserName, {credit_card, ok}),
              loop(NewData, UserName);
            false ->
              reply(Pid, UserName, {credit_card, {error, card_invalid}}),
              loop(Data, UserName)
          end
      end ;

    {request, Pid, {buy}} ->
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
                          reply(Pid, UserName, {buy, {ok, {Cart, TotalPrice}}}),
                          loop(NewData, UserName);
                        {error, _} ->
                          reply(Pid, UserName, {buy, {error, credit_info}}),
                          loop(Data, UserName)
                      end ;
                    _ ->
                      reply(Pid, UserName, {buy, {error, billing_info}}),
                      loop(Data, UserName)
                  end ;
                false ->
                  reply(Pid, UserName, {buy, {error, credit_info}}),
                  loop(Data, UserName)
              end ;
            [] ->
              reply(Pid, UserName, {buy, {error, credit_info}})
          end
      end
  end.