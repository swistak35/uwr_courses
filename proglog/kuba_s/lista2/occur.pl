%(+,+,?)
occurences(T,T,1) :- !.
occurences(S,T,N) :-
	T =.. [F|FS],
	occList(S,FS,N1),
	(
		F = S, N is N1+1 ;
		F \= S, N = N1
	).	
	
occList(_,[],0).
occList(S,[H|T],N) :-
	occurences(S,H,N1),
	occList(S,T,N2),
	N is N1+N2.
	
