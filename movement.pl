:- module(movement, []).

:- use_module(pawn, [move//1 as pawn_move]).

best(State, Move) :- phrase(pawn_move(State), [Move]).

forward(square(R, C), Steps, black, square(R1, C)) :-
  R1 is R - Steps, between(1, 8, R1).
forward(square(R, C), Steps, white, square(R1, C)) :-
  R1 is R + Steps, between(1, 8, R1).

move(move, square(SR, SC), square(DR,DC)) :-
  between(1,8,SR), between(1,8,SC),
  between(1,8,DR), between(1,8,DC).
