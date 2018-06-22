:- module(knight, []).

:- use_module('../board').
:- use_module('../state').
:- use_module(positions).

%% move(+Board: board, +Turn: turn, +move:move: -move:move).
%
%  Formulates a capture move.
%
%  @param Board the current board
%  @param Turn the owner of the knight
%  @param move the unprocessed move
%  @param move(capture) the capturing move
move(Board, Turn, move(From, To), move(capture, From, To)) :-
  % Verify the destination square contains an enemy piece.
  board:enemy(Board, To, Turn), !.

%% move(+Board: board, +Turn: turn, +move:move: -move:move).
%
%  Formulates a regular walking move.
%
%  @param Board the current board
%  @param Turn the owner of the knight
%  @param move the unprocessed move
%  @param move(move) the walking move
move(Board, _, move(From, To), move(move, From, To)) :-
  % Verify the destination square does not contain any piece.
  board:free(Board, To).

%% moves(+State: state, +Square: square, +Turn: turn, -Moves: list).
%
%  Finds all valid moves for a knight on the given Square.
%
%  @param State the current game state
%  @param Square the square that contains a knight
%  @param Turn the owner of the knight
%  @param Moves the resulting available moves
moves(State, Square, Turn, Moves) :-
  % Extract the board from the state.
  state:board(State, Board),
  % Get all knight moves from the current square.
  positions:knight_moves(Square, KnightMoves),
  % Convert the moves to either a move or a capture.
  convlist(move(Board, Turn), KnightMoves, Moves).
