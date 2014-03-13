-module(webclient).

%% API
-export([reply/2]).

reply(UserName, Message) ->
  case Message of
    {ski, [NewStatus, {total, NewCount}]} ->
      output_action_description(NewStatus, NewCount, "skis", UserName),
      ok;

    {bike, [NewStatus, {total, NewCount}]} ->
      output_action_description(NewStatus, NewCount, "bikes", UserName),
      ok;

    {surfboard, [NewStatus, {total, NewCount}]} ->
      output_action_description(NewStatus, NewCount, "surfboards", UserName),
      ok;

    {skateboard, [NewStatus, {total, NewCount}]} ->
      output_action_description(NewStatus, NewCount, "skateboards", UserName),
      ok;

    {view_cart, {[{ski, N1}, {bike, N2}, {surfboard, N3}, {skateboard, N4}], TotalPrice}} ->
      io:format("The shopping cart for ~s is:~n~n", [UserName]),
      io:format("ski: ~p~nbike: ~p~nsurfboard: ~p~nskateboard: ~p~n~n", [N1, N2, N3, N4]),
      io:format("Total Price: ~s~.2f~n", [[163], TotalPrice]),
      ok;

    {billing_address, _} ->
      ok;

    {credit_card, _} ->
      ok;

    {buy, {ok, {Cart, TotalPrice}}} ->
      io:format("~s just bought ", [UserName]),
      output_items_list(Cart),
      io:format(". The total price is ~s~.2f.~n", [[163], TotalPrice]),
      ok;

    _ ->
      io:format("~n~n*****************************~n~n~nUNMANAGED REPLY:~n~s: ~p~n~n~n*****************************~n~n~n", [UserName, Message])
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