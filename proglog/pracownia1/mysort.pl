% split([], _, [], []):-
%   !.
% split([E|List], Med, Small, [E|Big]):-
%   E >= Med,
%   !,
%   split(List, Med, Small, Big).
% split([E|List], Med, [E|Small], Big):-
%   split(List, Med, Small, Big).
%

greater_than(E, X) :-
  X =< E.

qsort(List, Sorted):-
  qsort(List, Sorted, []).

qsort([], Sorted, Sorted).
qsort([E|Tail], Sorted, Acc):-
  % split(Tail, E, Small, Big),
  partition(greater_than(E), Tail, Small, Big),
  qsort(Big, NextAcc, Acc),
  qsort(Small, Sorted, [E|NextAcc]).

mysort(L, S) :- qsort(L, S).
