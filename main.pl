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

  fen:parse(FenRevRaw, State),

  atom_codes(FenRev, FenRevRaw),

  write(FenRev),

  % fen:parse(ResultString, State),

  halt(0).
