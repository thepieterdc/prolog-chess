:- module(movement, []).

forward(square(R, C), Steps, black, square(R1, C)) :-
  R1 is R - Steps, between(1, 8, R1).
forward(square(R, C), Steps, white, square(R1, C)) :-
  R1 is R + Steps, between(1, 8, R1).
