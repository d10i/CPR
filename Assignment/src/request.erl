-module(request).

-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([call/2, stop/1]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

%% API
start_link(ReferenceId) ->
  gen_server:start(?MODULE, [ReferenceId], []).

call(RequestPid, Message) ->
  gen_server:call(RequestPid, Message).

stop(RequestPid) ->
  gen_server:call(RequestPid, stop).

%% gen_server callbacks

-record(state, {reference_id, username, data}).

init([ReferenceId]) ->
  S = case db_server:read(ReferenceId) of
    {ok, {UserName, Data}} -> #state{reference_id = ReferenceId, username = UserName, data = Data};
    {error, _} -> #state{reference_id = ReferenceId}
  end,
  {ok, S}.

handle_call({start_link, UserName}, _From, S = #state{reference_id = ReferenceId}) ->
  db_server:write(ReferenceId, {UserName, new_data()}),
  reply(start_link, {ok, ReferenceId}, S);

handle_call({ski, N}, _From, S = #state{reference_id = ReferenceId, username = UserName, data = Data}) ->
  case Data of
    {[{ski, M}, R2, R3, R4], Ba, Cc} ->
      NewCount = max(M + N, 0),
      NewData = {[{ski, NewCount}, R2, R3, R4], Ba, Cc},
      NewStatus = new_status(M, NewCount),
      db_server:write(ReferenceId, {UserName, NewData}),
      reply(ski, [NewStatus, {total, NewCount}], S)
  end ;

handle_call({bike, N}, _From, S = #state{reference_id = ReferenceId, username = UserName, data = Data}) ->
  {ok, {UserName, Data}} = db_server:read(ReferenceId),
  case Data of
    {[R1, {bike, M}, R3, R4], Ba, Cc} ->
      NewCount = max(M + N, 0),
      NewData = {[R1, {bike, NewCount}, R3, R4], Ba, Cc},
      NewStatus = new_status(M, NewCount),
      db_server:write(ReferenceId, {UserName, NewData}),
      reply(bike, [NewStatus, {total, NewCount}], S)
  end ;

handle_call({surfboard, N}, _From, S = #state{reference_id = ReferenceId, username = UserName, data = Data}) ->
  {ok, {UserName, Data}} = db_server:read(ReferenceId),
  case Data of
    {[R1, R2, {surfboard, M}, R4], Ba, Cc} ->
      NewCount = max(M + N, 0),
      NewData = {[R1, R2, {surfboard, NewCount}, R4], Ba, Cc},
      NewStatus = new_status(M, NewCount),
      db_server:write(ReferenceId, {UserName, NewData}),
      reply(surfboard, [NewStatus, {total, NewCount}], S)
  end ;

handle_call({skateboard, N}, _From, S = #state{reference_id = ReferenceId, username = UserName, data = Data}) ->
  {ok, {UserName, Data}} = db_server:read(ReferenceId),
  case Data of
    {[R1, R2, R3, {skateboard, M}], Ba, Cc} ->
      NewCount = max(M + N, 0),
      NewData = {[R1, R2, R3, {skateboard, NewCount}], Ba, Cc},
      NewStatus = new_status(M, NewCount),
      db_server:write(ReferenceId, {UserName, NewData}),
      reply(skateboard, [NewStatus, {total, NewCount}], S)
  end ;

handle_call({view_cart}, _From, S = #state{reference_id = ReferenceId, username = UserName, data = Data}) ->
  {ok, {UserName, Data}} = db_server:read(ReferenceId),
  case Data of
    {Cart, _, _} ->
      reply(view_cart, {Cart, total_price(Cart)}, S)
  end ;

handle_call({billing_address, BillingAddress}, _From, S = #state{reference_id = ReferenceId, username = UserName, data = Data}) ->
  {ok, {UserName, Data}} = db_server:read(ReferenceId),
  case address_verifier:error_items(BillingAddress) of
    [] ->
      case Data of
        {Cart, _, Cc} ->
          NewData = {Cart, BillingAddress, Cc},
          db_server:write(ReferenceId, {UserName, NewData}),
          reply(billing_address, ok, S)
      end ;
    Items ->
      reply(billing_address, {error, Items}, S)
  end ;

handle_call({credit_card, CardNumber, {ExpYear, ExpMonth}}, _From, S = #state{reference_id = ReferenceId, username = UserName, data = Data}) ->
  {ok, {UserName, Data}} = db_server:read(ReferenceId),
  case Data of
    {Cart, BillingAddress, _} ->
      case cc:is_valid(BillingAddress, CardNumber, {ExpYear, ExpMonth}) of
        true ->
          NewData = {Cart, BillingAddress, [CardNumber, {ExpYear, ExpMonth}]},
          db_server:write(ReferenceId, {UserName, NewData}),
          reply(credit_card, ok, S);
        false ->
          reply(credit_card, {error, card_invalid}, S)
      end
  end ;

handle_call({buy}, _From, S = #state{reference_id = ReferenceId, username = UserName, data = Data}) ->
  {ok, {UserName, Data}} = db_server:read(ReferenceId),
  case Data of
    {Cart, BillingAddress, Cc} ->
      case Cc of
        [CardNumber, {ExpYear, ExpMonth}] ->
          case cc:is_valid(BillingAddress, CardNumber, {ExpYear, ExpMonth}) of
            true ->
              case address_verifier:is_valid(BillingAddress) of
                true ->
                  TotalPrice = total_price(Cart),
                  case cc:transaction(BillingAddress, CardNumber, {ExpYear, ExpMonth}, TotalPrice) of
                    {ok, _} ->
                      NewData = new_data(),
                      db_server:write(ReferenceId, {UserName, NewData}),
                      reply(buy, {ok, {Cart, TotalPrice}}, S);
                    {error, _} ->
                      reply(buy, {error, credit_info}, S)
                  end ;
                false ->
                  reply(buy, {error, billing_info}, S)
              end ;
            false ->
              reply(buy, {error, credit_info}, S)
          end ;
        [] ->
          reply(buy, {error, credit_info}, S)
      end
  end;

handle_call(stop, _From, State) ->
  {stop, normal, ok, State};

handle_call(_Msg, _From, State) ->
  {noreply, State}.

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

reply(Action, Reply, #state{reference_id = ReferenceId, username = UserName}) ->
  webclient:reply(UserName, {Action, Reply}),
  {stop, normal, Reply, ReferenceId}.

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