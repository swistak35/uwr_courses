squeeze([], SqueezedList, SqueezedList) :- !.
squeeze([CurrentAtom|T], [[CurrentAtom, CurrentCount]|TAcc], SqueezedList) :-
  !,
  NextCount is CurrentCount + 1,
  squeeze(T, [[CurrentAtom, NextCount]|TAcc], SqueezedList).
squeeze([NextAtom|T], Acc, SqueezedList) :-
  squeeze(T, [[NextAtom, 1]|Acc], SqueezedList).

convert2([], void).
convert2([[Atom, Count]|T], bag(Atom, Count, BagsTail)) :-
  convert2(T, BagsTail).

convert(List, Bags) :-
  msort(List, SortedList),
  squeeze(SortedList, [], SqueezedList),
  convert2(SqueezedList, Bags).
