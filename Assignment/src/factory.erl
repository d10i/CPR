-module(factory).

%% API
-export([start_link/1, ski/2, bike/2, surfboard/2, skateboard/2, view_cart/1, billing_address/2, credit_card/3, buy/1]).
-export([init/0]).
-export([start/0, stop/0]).

stop() -> call(stop).
start_link(UserName) -> call({start_link, UserName}).
ski(ReferenceId, N) -> call(ReferenceId, {ski, N}).
bike(ReferenceId, N) -> call(ReferenceId, {bike, N}).
surfboard(ReferenceId, N) -> call(ReferenceId, {surfboard, N}).
skateboard(ReferenceId, N) -> call(ReferenceId, {skateboard, N}).
view_cart(ReferenceId) -> call(ReferenceId, {view_cart}).
billing_address(ReferenceId, BillingAddress) -> call(ReferenceId, {billing_address, BillingAddress}).
credit_card(ReferenceId, CardNumber, ExpirationDate) -> call(ReferenceId, {credit_card, CardNumber, ExpirationDate}).
buy(ReferenceId) -> call(ReferenceId, {buy}).

start() ->
  register(factory, spawn(factory, init, [])),
  cc:start(),
  ok.

init() ->
  loop(db:new()).

call(Message) ->
  factory ! {request, self(), Message},
  receive
    {reply, Reply} -> Reply
  end.

call(ReferenceId, Message) ->
  factory ! {request, self(), ReferenceId, Message},
  receive
    {reply, Reply} -> Reply
  end.

reply(Pid, Message) ->
  Pid ! {reply, Message}.

get_session(ReferenceId, Db) ->
  case db:read(ReferenceId, Db) of
    {ok, Session} -> Session;
    {error, _} -> error
  end.

loop(Db) ->
  receive
    {request, Pid, stop} ->
      % TODO: stop all sessions?
      cc:stop(),
      reply(Pid, ok);

    {request, Pid, {start_link, UserName}} ->
      ReferenceId = make_ref(),
      Session = spawn(session, init, [UserName]),
      NewDb = db:write(ReferenceId, Session, Db),
      reply(Pid, {ok, ReferenceId}),
      loop(NewDb);

    {request, Pid, ReferenceId, Message} ->
      Session = get_session(ReferenceId, Db),
      reply(Pid, session:call(Session, Message)),
      loop(Db)
  end.