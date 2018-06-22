:- module(minimax, []).

:- use_module(movement).
:- use_module(state).

%% adjust_depth(+Moves: list, +Depth: integer, -NewDepth: integer).
%
%  Adjusts the depth based on the amount of moves. If the amount of moves is
%  very small, the tree is allowed to go deeper one more level to possibly
%  choose a better move.
%
%  @param Moves the moves
%  @param Depth the current depth
%  @param the adjusted depth
adjust_depth(Moves, Depth, NewDepth) :-
  % Get the amount of moves.
  length(Moves, I),
  I < 4,
  NewDepth is Depth + 1, !.
adjust_depth(_, Depth, Depth).

%% alphabeta(+InitialState: state, +MaxDepth: integer, -BestState: state).
%
%  Entry point for the alpha-beta pruning minimax function.
%
%  @param InitialState the current state
%  @param MaxDepth the maximum depth to search
%  @param BestState the best move
alphabeta(InitialState, MaxDepth, BestState) :-
  % Get the current turn frmo the game state.
  state:turn(InitialState, Player),
  % Start the alpha-beta pruning.
  alphabeta(InitialState, Player, MaxDepth, -999 999 999, +999 999 999, BestState, _).

%% alphabeta(+State: state, +Player: player, +Depth: integer, +Alpha: integer,
%%           +Beta: integer, -BestNextState: state, -BestScore: integer).
%
%  Alpha-beta pruning function.
%
%  @param State the current state
%  @param Player the current player
%  @param Depth the maximum remaining depth to search
%  @param Alpha the alpha bound
%  @param Beta the beta bound
%  @param BestNextState the best move
%  @param BestScore the best move's score
alphabeta(State, Player, 0, _, _, _, Score) :-
  % Base case, don't search any further.
  score(State, Player, Score), !.
alphabeta(Current, Player, Depth, Alpha, Beta, BestNextState, BestScore) :-
  % Generate all next moves.
  movement:all_moves(Current, NextMoves),
  % Get the adjusted depth.
  adjust_depth(NextMoves, Depth, NewDepth),
  % Apply all next moves.
  maplist(state:apply_move(Current), NextMoves, NextStates),
  % Find the best move.
  best(NextStates, Player, NewDepth, Alpha, Beta, BestNextState, BestScore), !.
alphabeta(State, Player, _, _, _, _, Score) :-
  % No more moves are available, return the score of the current state.
  score(State, Player, Score).

%% best(+States: list, +Player: player, +Depth: integer, +Alpha: integer,
%%      +Beta: integer, -BestState: state, -BestScore: integer).
%
%  Evaluate all states and get the best state (recursive step).
%
%  @param States the states to evaluate
%  @param Player the current player
%  @param Depth the current depth
%  @param Alpha the alpha bound
%  @param Beta the beta bound
%  @param BestState the best move
%  @param BestScore the best score
best([State1 | States], Player, Depth, Alpha, Beta, BestState, BestScore) :-
  % Decrease the depth.
  Deeper is Depth - 1,
  % Perform alpha-beta pruning on the next depth.
  alphabeta(State1, Player, Deeper, Alpha, Beta, _, Score1),
  % Evaluate to find the best move.
  evaluate(State1, Score1, States, Player, Depth, Alpha, Beta, BestState, BestScore).

%% better_of(+State1: state, +Score1: integer, +State2: state, +Score2: integer,
%%           -BestState: state, -BestScore: integer).
%
%  Get the best state between two states.
%
%  @param State1 the first state
%  @param Score1 the first score
%  @param State2 the second state
%  @param Score2 the second score
%  @param BestState the best move
%  @param BestScore the best score
better_of(State1, Score, _, Score, _, State1, Score) :-
  % Choose either one if both scores are equal.
  random(0, 2, 1), !.
better_of(_, Score, State2, Score, _, State2, Score).
better_of(State1, Score1, _, Score2, Player, State1, Score1) :-
  % Current player's turn -> Maximize.
  turn(State1, Player), Score1 > Score2, !.
better_of(State1, Score1, _, Score2, Player, State1, Score1) :-
  % Enemy's turn -> Minmize.
  \+ turn(State1, Player), Score1 < Score2, !.
better_of(_, _, State2, Score2, _, State2, Score2).

%% bounds(+Alpha: integer, +Beta: integer, +State: state, +Score: integer,
%%        +Player: turn, -NewAlpha: integer, -NewBeta: integer).
%
%  Update the alpha or beta bound.
%
%  @param Alpha the current alpha
%  @param Beta the current beta
%  @param State the State
%  @param Score the score for the given state
%  @param Player the player
%  @param NewAlpha the updated alpha value
%  @param NewBeta the updated beta value
bounds(Alpha, Beta, State, Score, Player, Score, Beta) :-
  % Verify if the alpha should be updated.
  turn(State, Player), Score > Alpha, !.
bounds(Alpha, Beta, State, Score, Player, Alpha, Score) :-
  % Verify if the alpha should be updated.
  \+ turn(State, Player), Score < Beta, !.
bounds(Alpha, Beta, _, _, _, Alpha, Beta).

%% evaluate(+State: state, +Score: integer, +States: list, +Player: turn,
%%          +Depth: integer, +Alpha: integer, +Beta: integer, -BestState: state,
%%          -BestScore: integer).
%
%  Evaluates the current state and determines the best one.
%
%  @param State the State
%  @param Score the score for the given state
%  @param State other states to evaluate
%  @param Player the player
%  @param Depth the current depth
%  @param Alpha the current alpha
%  @param Beta the current beta
%  @param BestState the best move
%  @param BestScore the best score
evaluate(State, Score, [], _, _, _, _, State, Score) :- !.
evaluate(State, Score, States, Player, Depth, Alpha, Beta, BestState, BestScore) :-
  % Calculate the new alpha and beta bounds.
  bounds(Alpha, Beta, State, Score, Player, NewAlpha, NewBeta),
  % Determine the next best move.
  best(States, Player, Depth, NewAlpha, NewBeta, State1, Score1),
  % Determine the best move between the lower depth and the current best.
  better_of(State, Score, State1, Score1, Player, BestState, BestScore).

%% piece_score(+Piece: piece, -Score: integer).
%
%  Get the minmax score for a given piece.
%
%  @param Piece the piece
%  @param Score the score
piece_score(bishop, 3 000).
piece_score(king, 1 000 000).
piece_score(knight, 12 000).
piece_score(pawn, 1 000).
piece_score(queen, 25 000).
piece_score(rook, 5 000).

%% score(+State: state, +Player: turn, -Score: integer).
%
%  Evaluate a given state and player.
%
%  @param State the State
%  @param Player the player
%  @param Score the score
score(State, Player, Score) :-
  % Get the enemy of the player.
  state:enemy(Player, Enemy),
  % Get this player's score.
  score_sub(State, Player, MyScore),
  % Get the enemy's score.
  score_sub(State, Enemy, EnemyScore),
  % Calculate the final score.
  Score is MyScore - EnemyScore.

%% score_sub(+State: state, +Player: turn, -Score: integer).
%
%  Evaluate a given state and player, subroutine.
%
%  @param State the State
%  @param Player the player
%  @param Score the score
score_sub(State, Player, Score) :-
  % Find all pieces of the player.
  findall(Type, state:piece_at(State, _, piece(Type, Player)), Pieces),
  % Map each piece to its score.
  maplist(piece_score, Pieces, PieceScores),
  % Calculate the sum of all the scores.
  sum_list(PieceScores, Score).

%% turn(+State: state, -Turn: turn).
%
%  Get the current player, this is the enemy of the player in the state, since
%  since the state has the next player (the move is already applied).
%
%  @param State the State
%  @param Turn the turn
turn(State, Turn) :-
  % Get the opposite of the turn in the state.
  \+ state:turn(State, Turn), !.
turn(State, Turn) :-
  % Get the turn from the state.
  state:turn(State, Player),
  % Get the enemy of the turn from the state.
  state:enemy(Player, Turn).
