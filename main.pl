#!/usr/bin/env swipl

:- set_prolog_flag(double_quotes, codes).

:- use_module(board).
:- use_module(draw).
:- use_module(fen).
:- use_module(movement).
:- use_module(state).

:- initialization(main).

parse(Argv, State) :-
  atomic_list_concat(Argv, ' ', FenRaw),
  atom_codes(FenRaw, FenString),
  fen:parse(FenString, State).

write_fen(State) :-
  fen:parse(ResultFen, State),
  atom_codes(ResultRaw, ResultFen),
  write(ResultRaw),nl.

% regular main
main(Argv) :-
  length(Argv, 6),

  parse(Argv, State),

  movement:random_move(State, Move),

  state:apply_move(State, Move, AfterState),

  write_fen(AfterState),

  halt(0).

% TEST main
main(Argv) :-
  length(Argv, 7),

  % verwijder het test arg uit de input
  include(\=('TEST'), Argv, ArgvClean),

  parse(ArgvClean, State),

  movement:all_moves(State, Moves),

  maplist(state:apply_move(State), Moves, ResultStates),

  maplist(write_fen(), ResultStates),

  halt(0).
