:- dynamic mypermK/3.
:- dynamic mypermW/3.

wiezowce(N, W, K, R) :-
  retractall(mypermK(_,_,_)),
  retractall(mypermW(_,_,_)),
  genBoard(N, Board),
  transp(Board, TransposedBoard),
  assert_all_perms(N, K, W),
  filling_row(Board, TransposedBoard, N, W, K),
  R = Board.

assert_all_perms(N, K, W) :-
  numlist(1, N, L),
  permutation(L, P),
  count_mono(P, CL),
  reverse(P, R),
  count_mono(R, CR),
  assert_for_mypermK(CL, CR, P, K),
  assert_for_mypermW(CL, CR, P, W),
  fail.
assert_all_perms(N, K, W).

assert_for_mypermK(CL, CR, P, K) :-
  member((CL, CR), K),
  assert(mypermK(CL, CR, P)),
  !.
assert_for_mypermK(CL, CR, P, K).
assert_for_mypermW(CL, CR, P, W) :-
  member((CL, CR), W),
  assert(mypermW(CL, CR, P)),
  !.
assert_for_mypermW(CL, CR, P, W).


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

count_mono(L, Res) :- count_mono(L, 0, 0, Res).
count_mono([], _Max, Acc, Acc).
count_mono([H|T], Max, Acc, Res) :-
  H > Max,
  !,
  NAcc is Acc + 1,
  count_mono(T, H, NAcc, Res).
count_mono([H|T], Max, Acc, Res) :-
  count_mono(T, Max, Acc, Res).

check_row_right(K, L) :-
  reverse(L, L2),
  check_row(K, L2, 0).

filling_row([], _ColumnBoard, _N, _W, _K).
filling_row([Current|RestRows], ColumnBoard, N, [(HLeft, HRight)|TW], K) :-
  mypermW(HLeft, HRight, Current),
  filling_col(RestRows, ColumnBoard, N, TW, K).

filling_col(_RowBoard, [], _N, _W, _K).
filling_col(RowBoard, [Current|RestCols], N, W, [(HTop, HBottom)|TK]) :-
  mypermK(HTop, HBottom, Current),
  filling_row(RowBoard, RestCols, N, W, TK).

genBoard(N, Board) :- genBoard(0, N, Board).
genBoard(N, N, []) :- !.
genBoard(K, N, [L|Board]) :-
  length(L, N),
  succ(K, K1),
  genBoard(K1, N, Board).
