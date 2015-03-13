term(S,N,T) :- 
	var(T) ->
		genTerm(S,N,N,T) ;
		checkTerm(S,T,N). % genTerm(S,10,N,T).

% (+S,+Max,?Size,?Term)
% Term jest termem nad S o wielkosci Size
% przy czym Size <= Max
genTerm(S,M,N,T) :-
	member(C/A,S),
	M1 is M-1,
	genTermList(S,A,M1,N1,L),
	N is N1+1,
	T =.. [C|L].
	
% (+S,+Len,+Max,?Size,?List)
% List jest lista termow nad S o dlugosci Len
% i sumarycznej wielkosci Size
% przy czym Size <= Max
genTermList(_,0,M,0,[]) :- 
	!,M >= 0.
genTermList(S,L,M,N,[T|R]) :-
	L > 0, M >= L,
	L1 is L-1,
	MT is M - L1,
	genTerm(S,MT,NT,T),
	M1 is M - NT,
	genTermList(S,L1,M1,N1,R),
	N is NT+N1.
	
% (+S,+Term,?Size)
% Term jest poprawnym termem nad S o wielkosci Size
checkTerm(S,T,N) :-
	T =.. [F|L],
	length(L,Len),
	member(F/Len,S),
	checkTermList(S,L,N1),
	N is N1+1.
	
checkTermList(_,[],0) :- !.
checkTermList(S,[H|T],N) :-
	checkTerm(S,H,NT),
	checkTermList(S,T,N1),
	N is N1 + NT.
