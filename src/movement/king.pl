:- module(king, []).

:- use_module('../board').
:- use_module('../movement').
:- use_module('../state').

%% castle(+State: state, +Square:, +Turn: turn, +move:move) is semidet
%
%  Formulates a castling move (kingside).
%
%  @param State the current game state
%  @param Square the current king square
%  @param Turn the owner of the king
%  @param move(castling) the castling move
castle(State, Square, Turn, move(castling, castling(kingside, Turn))) :-
  % Verify the king is on a valid castling square.
  board:castling_squares(castling(kingside, Turn), Square, RookSquare, _, _),
  % Extract the board from the state.
  state:board(State, Board),
  % Extract the castling possibilities from the state.
  state:castling(State, Castlings),
  % Verify this castling type may be executed.
  member(castling(kingside, Turn), Castlings),
  % Verify there is no piece between the king and the rook.
  movement:path_clear(Board, move(Square, left, RookSquare)).

%% castle(+State: state, +Square:, +Turn: turn, +move:move) is semidet
%
%  Formulates a castling move (queenside).
%
%  @param State the current game state
%  @param Square the current king square
%  @param Turn the owner of the king
%  @param move(castling) the castling move
castle(State, Square, Turn, move(castling, castling(queenside, Turn))) :-
  % Verify the king is on a valid castling square.
  board:castling_squares(castling(queenside, Turn), Square, RookSquare, _, _),
  % Extract the board from the state.
  state:board(State, Board),
  % Extract the castling possibilities from the state.
  state:castling(State, Castlings),
  % Verify this castling type may be executed.
  member(castling(queenside, Turn), Castlings),
  % Verify there is no piece between the king and the rook.
  movement:path_clear(Board, move(Square, left, RookSquare)).

%% move(+Board: board, +Turn: turn, +move:move: -move:move) is semidet
%
%  Formulates a capture move.
%
%  @param Board the current board
%  @param Turn the owner of the king
%  @param move the unprocessed move
%  @param move(capture) the capturing move
move(Board, Turn, move(From, To), move(capture, From, To)) :-
  % Verify the destination square contains an enemy piece.
  board:enemy(Board, To, Turn), !.

%% move(+Board: board, +Turn: turn, +move:move: -move:move) is semidet
%
%  Formulates a regular walking move.
%
%  @param Board the current board
%  @param Turn the owner of the king
%  @param move the unprocessed move
%  @param move(move) the walking move
move(Board, _, move(From, To), move(move, From, To)) :-
  % Verify the destination square does not contain any piece.
  board:free(Board, To).

%% moves(+State: state, +Square: square, +Turn: turn, -list) is semidet
%
%  Finds all valid moves for a knight on the given Square.
%
%  @param State the current game state
%  @param Square the square that contains a king
%  @param Turn the owner of the king
%  @param Moves the resulting available moves
moves(State, Square, Turn, [FilteredMoves, CastlingMoves]) :-
  % Extract the board from the state.
  state:board(State, Board),

  % Get all king moves from the current square.
  positions:king_moves(Square, KingMoves),
  % Convert the moves to either a move or a capture.
  convlist(move(Board, Turn), KingMoves, FilteredMoves),

  % Find all castling moves from the current square and game state.
  findall(C, castle(State, Square, Turn, C), CastlingMoves).
