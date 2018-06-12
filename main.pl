#!/usr/bin/env swipl

:- use_module(board).
:- use_module(fen).
:- use_module(state).

:- initialization(main).

main(Argv) :-
  concat_atom(Argv, ' ', FenString),

  fen:parse(FenString, State),

  state:board(State, Board),
  state:turn(State, Turn),
  state:castling(State, Castling),
  state:en_passant(State, EnPassant),
  state:half_count(State, HalfCount),
  state:full_count(State, FullCount),

  board:set_piece(Board, coordinate(1, 1), bishop(black), Board2),

  fen:fen_string(State, FenResult),
  % fen:print(FenResult, [Board2, Turn, Castling, EnPassant, HalfCount, FullCount]),

  write(FenResult),

  halt(0).
