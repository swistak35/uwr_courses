switch_if_legal(List, Prefix, [X, Y], Suffix, List) :-
  Y >= X.
switch_if_legal(List, Prefix, [X, Y], Suffix, UpdatedList) :-
  append(Prefix, [Y, X], Begin),
  append(Begin, Suffix, UpdatedList).

switch_pairs(List, UpdatedList) :-
  append(Prefix, [X, Y], Begin),
  append(Begin, Suffix, List),
  switch_if_legal(List, Begin, [X, Y], Suffix, UpdatedList).

bubsort(List, List) :-
  msort(List, List).
bubsort(List, Sorted) :-
  switch_pairs(List, UpdatedList),
  bubsort(UpdatedList, Sorted).
