updateBag(void, E, Count, bag(E, Count, void)) :-
  !.
updateBag(bag(E, PrevCount, T), E, Count, bag(E, NewCount, T)) :-
  NewCount is Count + PrevCount,
  !.
updateBag(bag(E1, PrevCount, T), E, Count, bag(E1, PrevCount, Result1)) :-
  updateBag(T, E, Count, Result1).
  

convert([], void).
convert([H|T], R) :-
  convert(T, R2),
  updateBag(R2, H, 1, R).
