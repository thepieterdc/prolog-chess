:- module(minimax, []).

:- use_module(movement).
:- use_module(state).

adjust_depth(Moves, Depth, NewDepth) :-
  length(Moves, I),
  I < 5,
  NewDepth is Depth + 1, !.
adjust_depth(_, Depth, Depth).

%start functie
alphabeta(InitialState, MaxDepth, BestState) :-
  state:turn(InitialState, Player),
  alphabeta(InitialState, Player, MaxDepth, -999 999 999, +999 999 999, BestState, _).

alphabeta(State, Player, 0, _, _, _, Score) :-
  score(State, Player, Score), !.

alphabeta(Current, Player, Depth, Alpha, Beta, BestNextState, BestScore) :-
  movement:all_moves(Current, NextMoves),
  adjust_depth(NextMoves, Depth, NewDepth),
  maplist(state:apply_move(Current), NextMoves, NextStates),
  best(NextStates, Player, NewDepth, Alpha, Beta, BestNextState, BestScore), !.

% geen moves meer
alphabeta(State, Player, _, _, _, _, Score) :-
  score(State, Player, Score).

best([State1 | States], Player, Depth, Alpha, Beta, BestState, BestScore) :-
  Deeper is Depth - 1,
  alphabeta(State1, Player, Deeper, Alpha, Beta, _, Score1),
  evaluate(State1, Score1, States, Player, Depth, Alpha, Beta, BestState, BestScore).

% equal scores
better_of(State1, Score, _, Score, _, State1, Score) :- random(0, 2, 0), !.
better_of(_, Score, State2, Score, _, State2, Score).

% my turn -> maximize
better_of(State1, Score1, _, Score2, Player, State1, Score1) :-
  turn(State1, Player),
  Score1 > Score2, !.

% enemy's turn -> minimize
better_of(State1, Score1, _, Score2, Player, State1, Score1) :-
  \+ turn(State1, Player),
  Score1 < Score2, !.

% the first score was not the desired one
better_of(_, _, State2, Score2, _, State2, Score2).

bounds(Alpha, Beta, State, Score, Player, Score, Beta) :-
  turn(State, Player),
  Score > Alpha, !.

bounds(Alpha, Beta, State, Score, Player, Alpha, Score) :-
  \+ turn(State, Player),
  Score < Beta, !.

bounds(Alpha, Beta, _, _, _, Alpha, Beta).

evaluate(State, Score, [], _, _, _, _, State, Score) :- !.

evaluate(State, Score, States, Player, Depth, Alpha, Beta, BestState, BestScore) :-
  bounds(Alpha, Beta, State, Score, Player, NewAlpha, NewBeta),
  best(States, Player, Depth, NewAlpha, NewBeta, State1, Score1),
  better_of(State, Score, State1, Score1, Player, BestState, BestScore).

piece_score(bishop, 3 000).
piece_score(king, 1 000 000).
piece_score(knight, 12 000).
piece_score(pawn, 1 000).
piece_score(queen, 25 000).
piece_score(rook, 5 000).

score(State, Player, Score) :-
  state:enemy(Player, Enemy),
  score_sub(State, Player, MyScore),
  score_sub(State, Enemy, EnemyScore),
  Score is MyScore - EnemyScore.

score_sub(State, Player, Score) :-
  findall(Type, state:piece_at(State, _, piece(Type, Player)), Pieces),
  maplist(piece_score, Pieces, PieceScores),
  sum_list(PieceScores, Score).

turn(State, Turn) :-
  \+ state:turn(State, Turn), !.

turn(State, Turn) :- state:turn(State, Player), state:enemy(Player, Turn).
