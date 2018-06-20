:- module(minimax, []).

:- use_module(movement).
:- use_module(state).

best([State], Player, Depth, State, Score) :-
  Deeper is Depth - 1,
  minimax(State, Player, Deeper, _, Score), !.

best([State1 | States], Player, Depth, BestState, BestScore) :-
  Deeper is Depth - 1,
  minimax(State1, Player, Deeper, _, Score1),
  best(States, Player, Depth, State2, Score2),
  betterOf(State1, Score1, State2, Score2, Player, BestState, BestScore).

% my turn -> maximize
betterOf(State1, Score1, _, Score2, Player, State1, Score1) :-
  state:turn(State1, Player),
  Score1 > Score2, !.

% enemy's turn -> minimize
betterOf(State1, Score1, _, Score2, Player, State1, Score1) :-
  state:turn(State1, Player),
  Score1 < Score2, !.

betterOf(_, _, State2, Score2, _, State2, Score2).

minimax(Initial, Depth, Next) :-
  state:turn(Initial, Player),
  minimax(Initial, Player, Depth, Next, _).

minimax(State, Player, 0, _, Score) :-
  score(State, Player, Score), !.

minimax(Current, Player, Depth, BestNextState, Score) :-
  movement:all_moves(Current, NextMoves),
  maplist(state:apply_move(Current), NextMoves, NextStates),
  best(NextStates, Player, Depth, BestNextState, Score), !.

% geen moves meer
minimax(State, Player, _, _, Score) :-
  score(State, Player, Score).

piece_score(bishop, 3).
piece_score(king, 1000).
piece_score(knight, 3).
piece_score(pawn, 1).
piece_score(queen, 9).
piece_score(rook, 5).

score(State, Player, Score) :-
  state:enemy(Player, Enemy),
  score_sub(State, Player, MyScore),
  score_sub(State, Enemy, EnemyScore),
  Score is MyScore - EnemyScore.

score_sub(State, Turn, Score) :-
  findall(Type, state:piece_at(State, _, piece(Type, Turn)), Pieces),
  maplist(piece_score, Pieces, PieceScores),
  sum_list(PieceScores, Score).
