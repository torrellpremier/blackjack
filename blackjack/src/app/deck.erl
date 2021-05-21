-module(deck).
-export([start/0,
          stop/1,
          deal_card/1]).

%% ETS Schema
-record(card, {name, suit, value, issued}).

%------------------------Public interface-------------------------%
start() ->
  TableID = ets:new(deck, [set, private]),
  try populate_table(TableID, [club, spade, heart, diamond], 1) of
    ok ->
      {ok, TableID}
  catch
    _:Error ->
      ets:delete(TableID),
      {error, Error}
  end.

stop(TableID) ->
  ets:delete(TableID),
  ok.

deal_card(TableID) ->
  case deal_card(TableID, rand:uniform(52)) of
    {ok, Card} ->
      {ok, Card};
    Error ->
      Error
  end.
%------------------------Public interface-------------------------%

%-----------------------Private functions------------------------%
populate_table(TableID, [Suit | Tail], Index) ->
  true = ets:insert_new(TableID, {Index, #card{name = ace, suit = Suit, value = 1, issued = false}}),
  true = ets:insert_new(TableID, {Index + 1, #card{name = '2', suit = Suit, value = 2, issued = false}}),
  true = ets:insert_new(TableID, {Index + 2, #card{name = '3', suit = Suit, value = 3, issued = false}}),
  true = ets:insert_new(TableID, {Index + 3, #card{name = '4', suit = Suit, value = 4, issued = false}}),
  true = ets:insert_new(TableID, {Index + 4, #card{name = '5', suit = Suit, value = 5, issued = false}}),
  true = ets:insert_new(TableID, {Index + 5, #card{name = '6', suit = Suit, value = 6, issued = false}}),
  true = ets:insert_new(TableID, {Index + 6, #card{name = '7', suit = Suit, value = 7, issued = false}}),
  true = ets:insert_new(TableID, {Index + 7, #card{name = '8', suit = Suit, value = 8, issued = false}}),
  true = ets:insert_new(TableID, {Index + 8, #card{name = '9', suit = Suit, value = 9, issued = false}}),
  true = ets:insert_new(TableID, {Index + 9, #card{name = '10', suit = Suit, value = 10, issued = false}}),
  true = ets:insert_new(TableID, {Index + 10, #card{name = jack, suit = Suit, value = 10, issued = false}}),
  true = ets:insert_new(TableID, {Index + 11, #card{name = queen, suit = Suit, value = 10, issued = false}}),
  true = ets:insert_new(TableID, {Index + 12, #card{name = king, suit = Suit, value = 10, issued = false}}),
  populate_table(TableID, Tail, Index + 13);

populate_table(_, [], _) ->
  ok.

deal_card(TableID, Index) ->
  case ets:lookup(TableID, Index) of
    [{_, {_, _, _, _, true}}] ->
      %%% Card has already been issued, grab another
      deal_card(TableID, rand:uniform(52));
    [{Index, {_, Card, Suit, Value, false}}] ->
      %% Card has yet to be issued, send this to the user
      ets:insert(TableID, {Index, #card{name = Card, suit = Suit, value = Value, issued = true}}),
      {ok, {Card, Suit, Value}};
    _Else ->
      {error, {invalid_index, Index}}
  end.

%-----------------------Private functions------------------------%
