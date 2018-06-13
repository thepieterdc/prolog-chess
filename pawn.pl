#!/usr/bin/env swipl

:- module(pawn, []).

:- use_module(board).
:- use_module(fen).
:- use_module(movement).
:- use_module(state).

:- initialization(main).

at(Board, Color, Square) :- board:piece_at(Board, Square, piece(pawn, Color)).

move(State) --> board:square(Square),
  {
    state:board(State, Board),
    state:turn(State, Turn),

    at(Board, Turn, Square),

    movement:forward(Square, 1, Turn, Destination),

    board:free(Board, Destination)
  },
  [Destination].

main(Argv) :-
  atomic_list_concat(Argv, ' ', FenRaw),
  atom_codes(FenRaw, FenString),

  fen:parse(FenString, State),

  phrase(pawn:move(State), Y),

  nl, write(Y),

  halt(0).
