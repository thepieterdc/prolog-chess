#!/usr/bin/env swipl

:- set_prolog_flag(gc, false).
:- set_prolog_flag(double_quotes, chars).
:- set_prolog_flag(stack_limit, 1 000 000 000).

:- use_module(fen).
:- use_module(minimax).
:- use_module(movement).
:- use_module(state).

:- initialization(main, main).

% filters checked states, bij solo is dit niet nodig want minimax fixt dit wel
filter(Player, State) :-
  \+ state:check(State, Player).

parse(Argv, State) :-
  atomic_list_concat(Argv, ' ', FenRaw),
  atom_codes(FenRaw, FenString),
  fen:parse(FenString, State).

write_draw() :-
  atom_codes(Draw, "DRAW"),
  write(Draw), nl.

write_fen(State) :-
  fen:parse(ResultFen, State),
  atom_codes(ResultRaw, ResultFen),
  write(ResultRaw), nl.

% regular main
main(Argv) :-
  length(Argv, 6),

  parse(Argv, State),

  minimax:alphabeta(State, 1, NextState),

  write_fen(NextState),

  halt(0).

% TEST main
main(Argv) :-
  length(Argv, 7),

  % verwijder het test arg uit de input
  include(\=('TEST'), Argv, ArgvClean),

  parse(ArgvClean, State),

  state:turn(State, Player),

  movement:all_moves(State, Moves),

  maplist(state:apply_move(State), Moves, ResultStates),
  length(ResultStates, AmountStates),
  % geen geldige moves dus draw
  AmountStates > 0,

  include(filter(Player), ResultStates, CheckedStates),

  maplist(write_fen(), CheckedStates), !,

  halt(0).

% when main fails we assume draw.
main(Argv) :-
  (length(Argv, 6) ; length(Argv, 7)),

  write_draw(),

  halt(0).
