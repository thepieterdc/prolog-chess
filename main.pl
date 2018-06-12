#!/usr/bin/env swipl

:- set_prolog_flag(double_quotes, codes).

:- use_module(board).
:- use_module(fen).
:- use_module(state).

:- initialization(main).

main(Argv) :-
  fen:parse("2Pp4/pppppppp/pppppppp/pppppppp/pppppppp/pppppppp/pppppppp/pppppppp b - - 0 1", State),

  write(State),

  halt(0).
