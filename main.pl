#!/usr/bin/env swipl

:- set_prolog_flag(double_quotes, codes).

:- use_module(board).
:- use_module(draw).
:- use_module(fen).
:- use_module(state).

:- initialization(main).

main(Argv) :-
  atomic_list_concat(Argv, ' ', FenRaw),
  atom_codes(FenRaw, FenString),

  fen:parse(FenString, State),

  state:board(State, Board),

  draw:drawBoard(Board),

  board:set_piece(Board, coordinate(1, 1), none, Board2),

  draw:drawBoard(Board2),

  halt(0).
