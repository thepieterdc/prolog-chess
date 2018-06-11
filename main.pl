#!/usr/bin/env swipl

:- use_module(parsing/fen).

:- initialization(main).

main(Argv) :-
  concat_atom(Argv, ' ', FenString),

  fen:parse(FenString, FenParsed),

  write(FenParsed),
  halt.
