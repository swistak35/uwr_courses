updateBag(void, E, Count, Result) :-
  Result = bag(E, Count, void),
  !.
updateBag(bag(E, PrevCount, T), E, Count, Result) :-
  NewCount is Count + PrevCount,
  Result = bag(E, NewCount, T),
  !.
updateBag(bag(E1, PrevCount, T), E, Count, Result) :-
  updateBag(T, E, Count, Result1),
  Result = bag(E1, PrevCount, Result1).
  


sum(B1, void, B1).
sum(B1, B2, BResult) :-
  B2 = bag(E, C, T),
  updateBag(B1, E, C, B12),
  sum(B12, T, BResult).

convert([], void).
convert([H|T], R) :-
  convert(T, R2),
  updateBag(R2, H, 1, R).

