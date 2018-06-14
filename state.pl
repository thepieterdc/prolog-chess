:- module(state, []).

apply_move(Before, move(move, From, To), After) :-
  board(Before, BeforeBoard),

  board:piece_at(BeforeBoard, From, MovedPiece),

  % Board
  board:move_piece(BeforeBoard, From, To, AfterBoard),
  update_board(Before, AfterBoard, BoardState),

  % Turn
  update_turn(BoardState, TurnState),

  % Half count
  inc_halfcount(TurnState, MovedPiece, HCState),

  % Full count
  inc_fullcount(HCState, After).

board([Board | _], Board).

castling([_, _, C | _], C).

en_passant([_, _, _, EP | _], EP).

full_count([_, _, _, _, _, FC], FC).

% white is the next player -> black played in this move
inc_fullcount([B, white, C, EP, HC, FC], [B, white, C, EP, HC, FC1]) :-
  succ(FC, FC1).
inc_fullcount([B, black, C, EP, HC, FC], [B, black, C, EP, HC, FC]).

half_count([_, _, _, _, HC | _], HC).

inc_halfcount([B, T, C, EP, _, FC], piece(pawn, _), [B, T, C, EP, 0, FC]) :- !.
inc_halfcount([B, T, C, EP, HC, FC], _, [B, T, C, EP, HC1, FC]) :-
  succ(HC, HC1).

turn([_, Turn | _], Turn).

update_board([_, T, C, EP, HC, FC], Board, [Board, T, C, EP, HC, FC]).

update_turn([B, white, C, EP, HC, FC], [B, black, C, EP, HC, FC]).
update_turn([B, black, C, EP, HC, FC], [B, white, C, EP, HC, FC]).
