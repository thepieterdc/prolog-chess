#!/usr/bin/env swipl

:- use_module(debugging/draw).
:- use_module(parsing/fen).

:- initialization(main).

main(Argv) :-
  concat_atom(Argv, ' ', FenString),

  fen:parse(FenString, Parsed),

  fen:board(Parsed, Board),

  draw:drawBoard(Board),

  halt(0).
