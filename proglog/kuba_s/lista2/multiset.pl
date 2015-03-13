convert([],void) :- !.
convert(L,S) :-
	msort(L,[H|T]),
	convert(T,H,1,S).
	
convert([],X,N,bag(X,N,void)).
convert([X|T],X,N,S) :-
	!, N1 is N+1,
	convert(T,X,N1,S).
convert([X|T],Y,N,bag(Y,N,S)) :-
	convert(T,X,1,S).
