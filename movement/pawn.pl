:- module(pawn, []).

:- use_module('../board').
:- use_module('../movement').
:- use_module('../state').

% Pawn capture.
capture(State, Turn, move(From, To), move(capture, From, To)) :-
  state:board(State, Board),
  \+ promotion_square(To),
  board:enemy(Board, To, Turn).

% Pawn capture+promotion.
capture(State, Turn, move(From, To), PromotionMoves) :-
  state:board(State, Board),
  promotion_square(To),
  board:enemy(Board, To, Turn),
  bagof(Move, promotion_move(Move, From, To), PromotionMoves).

% En passant capture, enemysquare is plaats van stuk dat naast mij staat dat ik gepakt heb
capture(State, Turn, move(SR/SC, DR/DC), move(en_passant, SR/SC, SR/DC, DR/DC)) :-
  state:board(State, Board),
  state:en_passant(State, DR/DC),

  % stuk dat geslagen moet worden staat naast mij.
  board:enemy(Board, SR/DC, Turn),

  board:free(Board, DR/DC).

% Pawn double moves.
move(Board, _, move(From, EP, To), move(move, From, EP, To)) :-
  board:free(Board, EP),
  board:free(Board, To).

% Pawn promotion.
move(Board, _, move(From, To), PromotionMoves) :-
  promotion_square(To),
  board:free(Board, To),
  bagof(Move, promotion_move(Move, From, To), PromotionMoves).

% Pawn walk.
move(Board, _, move(From, To), move(move, From, To)) :-
  \+ promotion_square(To),
  board:free(Board, To).

moves(State, Square, Turn, [Moves, Captures, EnPassants]) :-
  state:board(State, Board),

  positions:pawn(Square, Turn, PawnMove),
  convlist(move(Board, Turn), [PawnMove], Moves),

  findall(X, positions:pawn_capture(Square, Turn, X), PawnCaptures),
  convlist(capture(State, Turn), PawnCaptures, Captures),

  findall(X, positions:pawn_enpassant(Square, Turn, X), EnPassantMove),
  convlist(move(Board, Turn), EnPassantMove, EnPassants).

promotion_move(move(promotion, bishop, Square, Destination), Square, Destination).
promotion_move(move(promotion, knight, Square, Destination), Square, Destination).
promotion_move(move(promotion, queen, Square, Destination), Square, Destination).
promotion_move(move(promotion, rook, Square, Destination), Square, Destination).

% turn maakt niet uit want pion kan toch niet achteruit
promotion_square(1/_).
promotion_square(8/_).
