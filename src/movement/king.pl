:- module(king, []).

:- use_module('../board').
:- use_module('../movement').
:- use_module('../state').

% Castling Queenside
castle(State, Square, Turn, move(castling, castling(kingside, Turn))) :-
  board:castling_squares(castling(kingside, Turn), Square, RookSquare, _, _),

  state:board(State, Board),
  state:castling(State, Castlings),

  member(castling(kingside, Turn), Castlings),

  movement:path_clear(Board, move(Square, right, RookSquare)).

% Castling Queenside
castle(State, Square, Turn, move(castling, castling(queenside, Turn))) :-
  board:castling_squares(castling(queenside, Turn), Square, RookSquare, _, _),

  state:board(State, Board),
  state:castling(State, Castlings),

  member(castling(queenside, Turn), Castlings),

  movement:path_clear(Board, move(Square, left, RookSquare)).

% King capture.
move(Board, Turn, move(From, To), move(capture, From, To)) :-
  board:enemy(Board, To, Turn), !.

% King walk.
move(Board, _, move(From, To), move(move, From, To)) :-
  board:free(Board, To).

moves(State, Square, Turn, [FilteredMoves, CastlingMoves]) :-
  state:board(State, Board),

  positions:king_moves(Square, KingMoves),
  convlist(move(Board, Turn), KingMoves, FilteredMoves),

  findall(C, castle(State, Square, Turn, C), CastlingMoves).
