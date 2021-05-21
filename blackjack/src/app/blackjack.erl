-module(blackjack).
-behaviour(gen_server).

% public interface
-export([start/0,
          stop/1,
          connect/2,
          hit/2,
          cards/2,
          stick/2]).

%% gen_server callbacks
-export([init/1,
          terminate/2,
          handle_call/3,
          handle_cast/2]).

%------------------------Public interface-------------------------%
start() ->
  gen_server:start_link(?MODULE, [], []).

stop(Pid) ->
  gen_server:stop(Pid).

connect(Pid, UserName) ->
  gen_server:call(Pid, {connect, UserName}).

hit(Pid, UserName) ->
  gen_server:call(Pid, {hit, UserName}).

cards(Pid, UserName) ->
  gen_server:call(Pid, {cards, UserName}).

stick(Pid, UserName) ->
  gen_server:call(Pid, {stick, UserName}).
%------------------------Public interface-------------------------%

%----------------------gen_server callbacks-----------------------%
init([]) ->
  {ok, PlayerTableID} = players:start(),
  {ok, PlayerTableID}.

terminate(normal, PlayerTableID) ->
  {ok, PlayerList} = players:get_all_players(PlayerTableID),
  shutdown_decks(PlayerList),
  players:stop(PlayerTableID),
  ok.

handle_call({connect, UserName}, _From, PlayerTableID) ->
  case players:get_player(PlayerTableID, UserName) of
    {error, player_doesnt_exist} ->
      case players:add_player(PlayerTableID, UserName) of
        ok ->
          {reply, ok, PlayerTableID};
        Error ->
          {reply, Error, PlayerTableID}
        end;
    _Else ->
      {reply, {error, user_already_connected}, PlayerTableID}
  end;

handle_call({hit, UserName}, _From, PlayerTableID) ->
  case players:get_player(PlayerTableID, UserName) of
    {ok, {UserName, {player, DeckID, _, _, Status}}} ->
      case Status of
        lost ->
          {reply, {error, player_has_lost}, PlayerTableID};
        stuck ->
          {reply, {error, player_has_already_stuck_cards}, PlayerTableID};
        active ->
          case deck:deal_card(DeckID) of
            {ok, {Card, Suit, Value}} ->
              case players:update_player(PlayerTableID, UserName, Value, {Card, Suit, Value}) of
                {ok, {GameStatus, {_, NewScore}}} ->
                  {reply, {ok, {GameStatus, {new_card, {Card, Suit, Value}}, {new_score, NewScore}}}, PlayerTableID};
                Error ->
                  {reply, Error, PlayerTableID}
              end;
            Error ->
              {reply, Error, PlayerTableID}
          end
      end;
    Error ->
      {reply, Error, PlayerTableID}
  end;

handle_call({cards, UserName}, _From, PlayerTableID) ->
  case players:get_player(PlayerTableID, UserName) of
    {ok, {UserName, {player, _, Score, DealtCards, _}}} ->
      {reply, {ok, {score, Score}, {dealt_cards, DealtCards}}, PlayerTableID};
    Error ->
      {reply, Error, PlayerTableID}
  end;

handle_call({stick, UserName}, _From, PlayerTableID) ->
  case players:get_player(PlayerTableID, UserName) of
    {ok, {UserName, {player, _, Score, _, Status}}} ->
      case Status of 
        lost ->
          {reply, {error, player_has_lost}, PlayerTableID};
        stuck ->
          {reply, {error, player_has_already_stuck_cards}, PlayerTableID};
        active ->
          case players:stick_player(PlayerTableID, UserName) of
            ok ->
              {reply, {ok, {round_ended, {score, Score}}}, PlayerTableID};
            Error ->
              {reply, Error, PlayerTableID}
          end
      end;
    Error ->
      {reply, Error, PlayerTableID}
  end.

handle_cast(_, State) ->
  {noreply, State}.
%----------------------gen_server callbacks-----------------------%

%-----------------------Private functions------------------------%
shutdown_decks([{_, {player, DeckID, _, _, _}} | Tail]) ->
  deck:stop(DeckID),
  shutdown_decks(Tail);
shutdown_decks([]) ->
  ok.
%-----------------------Private functions------------------------%
