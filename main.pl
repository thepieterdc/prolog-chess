#!/usr/bin/env swipl

:- set_prolog_flag(double_quotes, codes).

:- use_module(board).
:- use_module(draw).
:- use_module(fen).
:- use_module(movement).
:- use_module(state).

:- initialization(main).

main(Argv) :-
  atomic_list_concat(Argv, ' ', FenRaw),
  atom_codes(FenRaw, FenString),

  fen:parse(FenString, State),

  state:board(State, Board),

  draw:drawBoard(Board),

  movement:best(State, Move), !,

  state:apply_move(State, Move, State2),

  state:board(State2, Board2),

  draw:drawBoard(Board2),

  fen:parse(ResultFen, State2),

  atom_codes(ResultRaw, ResultFen),

  write(ResultRaw),

  halt(0).
