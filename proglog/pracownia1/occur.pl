count([], _, 0).
count([E|T], E, C2) :-
  count(T, E, C1),
  succ(C1, C2).
count([H|T], E, C) :-
  H \= E,
  count(T, E, C).

occurences(S, T, C) :-
  T =.. [Name|Args],
  count(Args, S, CMain),
  maplist(occurences(S), Args, CArgsList),
  sum_list(CArgsList, CArgs),
  C is CMain + CArgs.

