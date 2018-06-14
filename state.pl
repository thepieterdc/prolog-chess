:- module(state, []).

apply_move(Before, move(move, From, To), After) :-
  board(Before, BeforeBoard),

  % Board
  board:move_piece(BeforeBoard, From, To, AfterBoard),
  update_board(Before, AfterBoard, BoardState),

  % Turn
  update_turn(BoardState, TurnState),

  % Half count
  increase_halfcount(TurnState, HCState),

  % Full count
  increase_fullcount(HCState, After).

board([Board | _], Board).

castling([_, _, C | _], C).

en_passant([_, _, _, EP | _], EP).

full_count([_, _, _, _, _, FC], FC).

% white is the next player -> black played in this move
increase_fullcount([B, white, C, EP, HC, FC], [B, white, C, EP, HC, FC1]) :-
  succ(FC, FC1).
increase_fullcount([B, black, C, EP, HC, FC], [B, black, C, EP, HC, FC]).

half_count([_, _, _, _, HC | _], HC).

increase_halfcount([B, T, C, EP, HC, FC], [B, T, C, EP, HC1, FC]) :-
  succ(HC, HC1).

turn([_, Turn | _], Turn).

update_board([_, T, C, EP, HC, FC], Board, [Board, T, C, EP, HC, FC]).

update_turn([B, white, C, EP, HC, FC], [B, black, C, EP, HC, FC]).
update_turn([B, black, C, EP, HC, FC], [B, white, C, EP, HC, FC]).
