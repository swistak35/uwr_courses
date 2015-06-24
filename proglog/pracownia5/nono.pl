nono(Rows, Cols, Board) :-
  length(Rows, M),
  length(Cols, N),
  genBoard(M, N, Board),
  transp(Board, TransposedBoard),
  % assert_all_perms(N, Rows),
  filling_row(Board, TransposedBoard, N, M, Rows, Cols).

dane(autko,
    [[4],[1,1,6],[1,1,6],[1,1,6],[4,9],[1,1],[1,1],[2,7,2],[1,1,1,1],[2,2]],
    [[4],[1,2],[1,1],[5,1],[1,2],[1,1],[5,1],[1,1],[4,1],[4,1],[4,2],[4,1],[4,1],[4,2],[4]]
).

% assert_all_perms(N, Rows, W) :-
%   numlist(1, N, L),
%   permutation(L, P),
%   count_mono(P, CL),
%   reverse(P, R),
%   count_mono(R, CR),
%   assert_for_mypermK(CL, CR, P, K),
%   assert_for_mypermW(CL, CR, P, W),
%   fail.
% assert_all_perms(N, K, W).

assert_all_perms(N, Rows) :-
  length(L, N),
  sort(Rows, Rows2),
  member(Row, Rows2),
  gen_row(L, Row),
  assert((myrow(Row, L))),
  fail.
%   numlist(1, N, L),
%   permutation(L, P),
%   count_mono(P, CL),
%   reverse(P, R),
%   count_mono(R, CR),
%   assert_for_mypermK(CL, CR, P, K),
%   assert_for_mypermW(CL, CR, P, W),
%   fail.
assert_all_perms(N, Rows).

one_fill([]).
one_fill([1|T]) :- one_fill(T).

add_gap([], []).
add_gap([0|T], T).

splice(List, P, Prefix, Suffix) :-
  length(Prefix, P),
  append(Prefix, Suffix, List).

gen_row([], []).
gen_row(List, [P|Rest]) :-
  splice(List, P, Prefix, Suffix),
  one_fill(Prefix),
  add_gap(Suffix, RestSuffix),
  gen_row(RestSuffix, Rest).
gen_row([0|T], Patterns) :-
  gen_row(T, Patterns).


filling_row([], [], N, M, Rows, Cols).
filling_row([Current|RestRows], [], N, M, [HRows|TRows], Cols) :-
  gen_row(Current, HRows),
  filling_col(RestRows, [], N, M, TRows, Cols).
filling_row([Current|RestRows], ColBoard, N, M, [HRows|TRows], Cols) :-
  gen_row(Current, HRows),
  filling_col(RestRows, ColBoard, N, M, TRows, Cols).
filling_col([], [], N, M, Rows, Cols).
filling_col([], [Current|RestCols], N, M, Rows, [HCols|TCols]) :-
  gen_row(Current, HCols),
  filling_col([], RestCols, N, M, Rows, TCols).
filling_col(RowBoard, [Current|RestCols], N, M, Rows, [HCols|TCols]) :-
  gen_row(Current, HCols),
  filling_row(RowBoard, RestCols, N, M, Rows, TCols).

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

% genBoard(M, N, Board) :- genBoard(M, N, Board).
genBoard(0, N, []) :- !.
genBoard(K, N, [L|Board]) :-
  length(L, N),
  succ(K1, K),
  genBoard(K1, N, Board).
