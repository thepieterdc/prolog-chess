:- module(minimax, []).

:- use_module(movement).
:- use_module(state).

best([State], Player, State, Score) :- minimax(State, Player, _, Score), !.

best([State1 | States], Player, Depth, BestState, BestScore) :-
  Deeper is Depth - 1,
  write(Deeper),
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

minimax(Initial, _, 0, _, Score) :-
  score(Initial, Score), !.

minimax(Current, Player, Depth, BestNextState, Score) :-
  movement:all_moves(Current, NextMoves),
  maplist(state:apply_move(Current), NextMoves, NextStates),
  best(NextStates, Player, Depth, BestNextState, Score), !.

% geen moves meer
minimax(Initial, _, _, _, Score) :-
  score(Initial, Score).

score(_, 0).
