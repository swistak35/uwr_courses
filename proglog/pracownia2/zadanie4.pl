switchpairs([H1,H2|T], 0, [H2,H1|T]) :-
  H1 > H2,
  !.
switchpairs(L, 0, L) :-
  !.
switchpairs([H|T1], C1, [H|T2]) :-
  C2 is C1 - 1,
  switchpairs(T1, C2, T2).

randsort3(List, _RandSize, List) :-
  msort(List, List),
  !.

randsort3(List, RandSize, Sorted) :-
  random(0, RandSize, Chosen),
  switchpairs(List, Chosen, UpdatedList),
  randsort3(UpdatedList, RandSize, Sorted).

randsort(List, Sorted) :-
  length(List, ListSize),
  RandSize is ListSize - 1,
  randsort3(List, RandSize, Sorted).

