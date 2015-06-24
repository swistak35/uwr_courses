wiezowce(N, W, K, R) :-
  genBoard(N, Board),
  transp(Board, TransposedBoard),
  filling_row(Board, TransposedBoard, N, W, K),
  R = Board.

fill_list(L, N) :-
  numlist(1, N, K),
  permutation(K, L).

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
  K1 is K - 1,
  check_row(K1, T, H).
check_row(K, [_H|T], Max) :-
  check_row(K, T, Max).
check_row(0, [], _) :- !.

check_row_right(K, L) :-
  reverse(L, L2),
  check_row(K, L2, 0).

filling_row([], _ColumnBoard, _N, _W, _K).
filling_row([Current|RestRows], ColumnBoard, N, [(HLeft, HRight)|TW], K) :-
  fill_list(Current, N),
  check_row(HLeft, Current, 0),
  check_row_right(HRight, Current),
  filling_col(RestRows, ColumnBoard, N, TW, K).

filling_col(_RowBoard, [], _N, _W, _K).
filling_col(RowBoard, [Current|RestCols], N, W, [(HTop, HBottom)|TK]) :-
  fill_list(Current, N),
  check_row(HTop, Current, 0),
  check_row_right(HBottom, Current),
  filling_row(RowBoard, RestCols, N, W, TK).

genBoard(N, Board) :- genBoard(0, N, Board).
genBoard(N, N, []) :- !.
genBoard(K, N, [L|Board]) :-
  length(L, N),
  succ(K, K1),
  genBoard(K1, N, Board).
