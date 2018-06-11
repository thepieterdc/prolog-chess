:- module(fen, [parse/2]).

:- use_module(board, [parse/2 as board_parse]).
:- use_module(turn, [parse/2 as turn_parse]).

parse(FenStr, [Board, Turn, nothing, nothing, HalfCount, FullCount]) :-
  split_string(FenStr, " ", "", [FBoard, FTurn, _, _, FHalf, FFull]),

  board_parse(FBoard, Board),

  turn_parse(FTurn, Turn),

  atom_number(FHalf, HalfCount),

  atom_number(FFull, FullCount).

board([Board, _, _, _, _, _], Board).
turn([_, Turn, _, _, _, _], Turn).
halfCount([_, _, _, _, HC, _], HC).
fullCount([_, _, _, _, _, FC], FC).
