:- module(queen, []).

:- use_module('../board').
:- use_module('../state').
:- use_module(positions).

%% move(+Board: board, +Turn: turn, +move:move: +move:move) is semidet
%
%  Formulates a capture move.
%
%  @param Board the current board
%  @param Turn the owner of the queen
%  @param move the unprocessed move
%  @param move(capture) the capturing move
move(Board, Turn, move(From, _, To), move(capture, From, To)) :-
  % Verify the destination square contains an enemy piece.
  board:enemy(Board, To, Turn), !.

%% move(+Board: board, +Turn: turn, +move:move: +move:move) is semidet
%
%  Formulates a regular walking move.
%
%  @param Board the current board
%  @param Turn the owner of the queen
%  @param move the unprocessed move
%  @param move(move) the walking move
move(Board, _, move(From, _, To), move(move, From, To)) :-
  % Verify the destination square does not contain any piece.
  board:free(Board, To).

%% moves(+State: state, +Square: square, +Turn: turn, -Moves: list) is semidet
%
%  Finds all valid moves for a queen on the given Square.
%
%  @param State the current game state
%  @param Square the square that contains a queen
%  @param Turn the owner of the queen
%  @param Moves the resulting available moves
moves(State, Square, Turn, Moves) :-
  % Extract the board from the state.
  state:board(State, Board),
  % Get all queen moves from the current square.
  positions:queen_moves(Square, QueenMoves),
  % Validate every found queen move.
  include(movement:path_clear(Board), QueenMoves, FilteredMoves),
  % Convert the moves to either a move or a capture.
  convlist(move(Board, Turn), FilteredMoves, Moves).
