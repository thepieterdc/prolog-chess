#!/usr/bin/env swipl

:- use_module(fen).
:- use_module(minimax).
:- use_module(movement).
:- use_module(state).

% Set the correct string mode.
:- set_prolog_flag(double_quotes, chars).
% Increase the stack limit to allow deeper searches.
:- set_prolog_flag(stack_limit, 2 000 000 000).
% Set the initialization goal.
:- initialization(main, main).

%% parse(+Argv:list, -State:state).
%
%  Parses the given FEN input arguments into a state.
%
%  @param Argv the arguments to parse
%  @param State the resulting state
parse(Argv, State) :-
  % Concatenate all the separate arguments into a FEN string.
  atomic_list_concat(Argv, ' ', FenRaw),
  % Convert the string to the correct representation.
  atom_codes(FenRaw, FenString),
  % Parse the FEN string into a state.
  fen:parse(FenString, State).

%% validate(+Player:turn, +State:state).
%
%  Validates a given state. A state is valid if the given player is not in
%  check.
%
%  @param Player the player's king that may not be attacked
%  @param State the state to validate
validate(Player, State) :-
  % Validate the player is not in check.
  \+ state:check(State, Player).

%% write_draw().
%
%  Writes out the "DRAW" message.
write_draw() :-
  % Convert the string "DRAW" to a list of atoms that can be printed.
  atom_codes(Draw, "DRAW"),
  % Print the string.
  write(Draw), nl.

%% write_fen(+State:state).
%
%  Writes out a given state in FEN notation.
%
%  @param State the state to print
write_fen(State) :-
  % Parse the state to its corresponding FEN notation.
  fen:parse(ResultFen, State),
  % Convert the FEN string to atoms that can be printed.
  atom_codes(ResultRaw, ResultFen),
  % Write out the result.
  write(ResultRaw), nl.

%% main(+Argv:list).
%
%  Regular main function that prints out the best move. This function will, if
%  successful, halt the program.
%
%  @param Argv the input arguments
main(Argv) :-
  % Precondition for this function: FEN has 6 parts.
  length(Argv, 6),
  % Parse the arguments into a state.
  parse(Argv, State),
  % Disable the garbage collector during calculations.
  set_prolog_flag(gc, false),
  % Perform minimax and alpha-beta pruning on the state to get the next best
  % state for the current player.
  minimax:alphabeta(State, 3, NextState),
  % Re-instantiate the garbage collector.
  set_prolog_flag(gc, false),
  % Write out the resulting move.
  write_fen(NextState),
  % Halt execution.
  halt(0).

%% main(+Argv:list).
%
%  Testing main function that prints out all next moves. This function will, if
%  successful, halt the program.
%
%  @param Argv the input arguments
main(Argv) :-
  % Precondition: there should be 6 FEN arguments and 1 TEST argument.
  length(Argv, 7),
  % Remove the TEST argument from the input.
  exclude(=('TEST'), Argv, ArgvClean),
  % Parse the arguments into a state.
  parse(ArgvClean, State),
  % Get the current player.
  state:turn(State, Player),
  % Generate all possible moves.
  movement:all_moves(State, Moves),
  % Apply all possible moves to obtain the resulting states.
  maplist(state:apply_move(State), Moves, ResultStates),
  % Remove the states where the player is in check.
  include(filter(Player), ResultStates, ValidStates),
  % Verify at least one legal move is available.
  length(ValidStates, AmountStates), AmountStates > 0,
  % Write the FEN representation for every valid state.
  maplist(write_fen(), ValidStates), !,
  % Halt execution.
  halt(0).

%% main(+Argv:list).
%
%  Main function that prints out a draw. A draw is assumed if the regular and
%  testing main functions have not found any valid moves.
%
%  @param Argv the input arguments
main(Argv) :-
  % The amount of input arguments must be 6 or 7 (testing).
  (length(Argv, 6) ; length(Argv, 7)),
  % Write out a draw.
  write_draw(),
  % Halt execution.
  halt(0).
