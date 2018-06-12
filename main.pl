#!/usr/bin/env swipl

:- use_module(board).
:- use_module(fen).
:- use_module(state).

:- initialization(main).

main(Argv) :-
  concat_atom(Argv, ' ', FenString),

  fen:parse(FenString, State),

  state:board(State, Board),

  write(Board), nl, nl,

  board:set_piece(Board, coordinate(1, 1), bishop(black), Board2),

  write(Board2),

  halt(0).
