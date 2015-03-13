% deterministyczny quicksort
% (+,?)
mysort([],[]).
mysort([H|T],Res) :-
	partition(H,T,[],[],Le,Gt),
	mysort(Le,R1),
	mysort(Gt,R2),
	append(R1,[H|R2],Res).
	
% (+Pivot,+List,+ALe,+AGt,?Le,?Gt)	
% Le = rev (filter (<=P) List) ++ ALe
% Gt = rev (filter (>P) List) ++ AGt
partition(_,[],ALe,AGt,ALe,AGt).
partition(P,[H|T],ALe,AGt,Le,Gt) :-
	(H =< P ->
		partition(P,T,[H|ALe],AGt,Le,Gt) ;
		partition(P,T,ALe,[H|AGt],Le,Gt)
	).
		
