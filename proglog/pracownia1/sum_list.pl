% sum_list([], 0).

% sum_list([H|T], X2) :-
%   sum_list(T, X1),
%   X2 is X1 + H.

sum_list(L, R) :- sum_list3(L, R, 0).
sum_list3([], R, R).
sum_list3([H|T], R, Acc) :-
  Acc2 is Acc + H,
  sum_list3(T, R, Acc2).
