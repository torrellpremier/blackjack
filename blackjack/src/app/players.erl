-module(players).
-export([start/0,
        stop/1,
        add_player/2,
        get_player/2,
        update_player/4,
        get_all_players/1,
        stick_player/2]).

-record(player, {deck_id, score, dealt_cards, status}).

%------------------------Public interface-------------------------%
start() ->
  TableID = ets:new(players, [set, private]),
  {ok, TableID}.

stop(TableID) ->
  ets:delete(TableID).

add_player(TableID, PlayerName) ->
  case ets:lookup(TableID, PlayerName) of
    [] ->
      case deck:start() of
        {ok, DeckID} ->
          ets:insert_new(TableID, {PlayerName, #player{deck_id = DeckID,
            score = 0, dealt_cards = [], status = active}}),
            ok;
        Error ->
          Error
      end;
    _Else ->
      {error, player_already_exists}
  end.

get_player(TableID, PlayerName) ->
  case ets:lookup(TableID, PlayerName) of
    [] ->
      {error, player_doesnt_exist};
    [PlayerInfo] ->
      {ok, PlayerInfo}
  end.

update_player(TableID, PlayerName, NewScore, DealtCard) ->
  case ets:lookup(TableID, PlayerName) of
    [] ->
      {error, player_doesnt_exist};
    [{PlayerName, {player, DeckID, Score, DealtCards, Status}}] ->
      case Status of
        lost ->
          {error, player_has_lost};
        active ->
          UpdatedDealtCards = lists:append(DealtCards, [DealtCard]),
          UpdatedScore = Score + NewScore,
          if
            UpdatedScore > 21 ->
              ets:insert(TableID, {PlayerName, #player{deck_id = DeckID,
                score = UpdatedScore, dealt_cards = UpdatedDealtCards, status = lost}}),
              {ok, {game_over, {PlayerName, UpdatedScore}}};
            true ->
              ets:insert(TableID, {PlayerName, #player{deck_id = DeckID,
                score = UpdatedScore, dealt_cards = UpdatedDealtCards, status = Status}}),
              {ok, {game_on, {PlayerName, UpdatedScore}}}
          end
      end
  end.

get_all_players(TableID) ->
  {ok, ets:tab2list(TableID)}.

stick_player(TableID, PlayerName) ->
  case ets:lookup(TableID, PlayerName) of
    [] ->
      {error, player_doesnt_exist};
    [{PlayerName, {player, DeckID, Score, DealtCards, Status}}] ->
      case Status of
        lost ->
          {error, player_has_lost};
        stuck ->
          {error, player_has_already_stuck_cards};
        active ->
          ets:insert(TableID, {PlayerName, #player{deck_id = DeckID,
            score = Score, dealt_cards = DealtCards, status = stuck}}),
            ok
        end
    end.
%------------------------Public interface-------------------------%

%-----------------------Private functions------------------------%
%-----------------------Private functions------------------------%
