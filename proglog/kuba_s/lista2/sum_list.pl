	%zad1
sum_list(L,R) :- sum(L,0,R).

% sum(+List,+Acc,?Res)
% Res = sum(List) + Acc
sum([],A,A).
sum([H|T],A,R) :-
	A1 is A+H,
	sum(T,A1,R).	
