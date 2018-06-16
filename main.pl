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

% regular main
main(Argv) :-
  length(Argv, 6),

  parse(Argv, State),

  state:board(State, Bord),

  draw:drawBoard(Bord),

  movement:all_moves(State, Moves),

  % state:apply_move(State, Move, State2),

  % fen:parse(ResultFen, State2),

  % atom_codes(ResultRaw, ResultFen),

  % write(ResultRaw),

  halt(0).

% TEST main
main(Argv) :-
  length(Argv, 7),

  % verwijder het test arg uit de input
  include(\=('TEST'), Argv, ArgvClean),

  parse(ArgvClean, State),

  state:board(State, Bord),

  draw:drawBoard(Bord),

  movement:all_moves(State, Moves),

  % state:apply_move(State, Move, State2),

  % fen:parse(ResultFen, State2),

  % atom_codes(ResultRaw, ResultFen),

  % write(ResultRaw),

  halt(0).
