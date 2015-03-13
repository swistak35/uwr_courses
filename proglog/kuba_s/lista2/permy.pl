%zad0 - select

select(X,L,[X|L]).
select(X,[H|T],[H|L]) :-
	select(X,T,L).

%zad0 - perm

% (+,?)
perm1([],[]).
perm1([H|T],L) :-
	perm1(T,T2),
	select(H,T2,L).
	
perm2([],[]).
perm2(L,[X|L2]) :-
	select(X,L1,L),
	perm2(L1,L2).

