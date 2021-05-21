# Blackjack Game using Erlang OTP

This is a blackjack game using the Erlang OTP framework.

## Instructios for use
1. Download rebar3
2. Navigate inside the ./blackjack/ directory
3. Run 'rebar3 shell'
4. Interact with the application as follows:
  * blackjack:start() - start the server -> Returns {ok, Pid}
  * blackjack:connect(Pid, <player_name_atom>) - connects a player to the game -> returns 'ok'
  * blackjack:hit(Pid, <player_name_atom>) - grabs a card from the deck -> returns {ok, {GameStatus, {new_card, {Card, Suit, Value}}, {new_score, Score}}}
  * blackjack:cards(Pid, <player_name_atom>) - shows all the cards that have been dealt to the player -> returns {ok, {score, Score}, {dealt_cards, ListOfDealtCards}}
  * blackjack:stick(Pid, <player_name_atom>) - ends the round for that player with the current score -> returns {ok, {round_ended, {score, Score}}}

## Additional Features
* Tracks the deck of cards
* Multiple clients can play multiple games independantly