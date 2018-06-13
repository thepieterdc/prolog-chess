#!/usr/bin/env swipl

:- module(pawn, []).

:- use_module(board).

:- initialization(main).

move(State) --> board:square(Square), write(Square).

main(_) :-
  phrase(move(pawn:move(X), Y)).
