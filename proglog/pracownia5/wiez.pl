% :- dynamic validrow/2.
% :- dynamic myperm/1.

% validrow(K, L) :- fail.

wiezowce(N, W, K, R) :-
  genBoard(N, Board),
  % assert_all_perms(N),
  transp(Board, TransposedBoard),
  filling_row(Board, Board, TransposedBoard, N, W, K),
  R = Board.

% assert_all_perms(N) :-
%   numlist(1, N, L),
%   permutation(L, P),
%   asserta(myperm(P)),
%   fail.
% assert_all_perms(N).

fill_list(L, N) :-
  numlist(1, N, K),
  permutation(K, L).
  % myperm(L).

%%% Transposing
transp([T], X) :-
  makeLists(T, X).
transp([Row|T], L2) :-
  transp(T, L1),
  append_to_all(Row, L1, L2).

makeLists([], []).
makeLists([H|T], [[H]|T1]):-
  makeLists(T, T1).

append_to_all([], [], []).
append_to_all([H1|T1], [H2|T2], [[H1|H2]|T3]) :-
  append_to_all(T1, T2, T3).

check_row(K, [H|T], Max) :-
  H > Max,
  !,
  % K1 is K - 1,
  succ(K1, K),
  K1 >= 0,
  check_row(K1, T, H).
check_row(K, [_H|T], Max) :-
  check_row(K, T, Max).
check_row(0, [], _) :- !.

% check_row_left(K, L) :-
%   validrow(K, L),
%   !.
% check_row_left(K, L) :-
%   invalidrow(K, L),
%   !,
%   fail.
check_row_left(K, L) :-
  check_row(K, L, 0).
  % check_row(K, L, 0),
  % asserta((validrow(K, L) :- !)),
  % !.
% check_row_left(K, L) :-
%   asserta((invalidrow(K, L) :- !)),
%   fail.
% check_row_right(K, L) :-
%   reverse(L, L2),
%   validrow(K, L2),
%   !.
check_row_right(K, L) :-
  reverse(L, L2),
  check_row(K, L2, 0).

filling_row(_Board, [], _ColumnBoard, _N, _W, _K).
filling_row(Board, [Current|RestRows], ColumnBoard, N, [(HLeft, HRight)|TW], K) :-
  fill_list(Current, N),
  check_row_left(HLeft, Current),
  check_row_right(HRight, Current),
  filling_col(Board, RestRows, ColumnBoard, N, TW, K).

filling_col(_Board, _RowBoard, [], _N, _W, _K).
filling_col(Board, RowBoard, [Current|RestCols], N, W, [(HTop, HBottom)|TK]) :-
  fill_list(Current, N),
  check_row_left(HTop, Current),
  check_row_right(HBottom, Current),
  filling_row(Board, RowBoard, RestCols, N, W, TK).

genBoard(N, Board) :- genBoard(0, N, Board).
genBoard(N, N, []) :- !.
genBoard(K, N, [L|Board]) :-
  length(L, N),
  succ(K, K1),
  genBoard(K1, N, Board).
