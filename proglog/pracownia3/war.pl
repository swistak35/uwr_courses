war(L1, L2, N) :-
	war(L1, L2, 0, N, L1, L2).

war(_L1, _L2, M, M, [], _K2).
war(_L1, _L2, M, M, _K1, []).

war(L1, L2, M, inf, L1, L2) :-
	M > 0,
	!.
war(L1, L2, M, R, K1, K2) :-
	duel(L1, L2, _X, [], NL1, NL2),
	duel(K1, K2, Y1, [], NK1, NK2),
	duel(NK1, NK2, Y2, [], NNK1, NNK2),
	M1 is M + Y1 + Y2,
	war(NL1, NL2, M1, R, NNK1, NNK2).

duel(L, [], 0, Acc, L1, []) :-
	append(L, Acc, L1).
duel([], L, 0, Acc, [], L1) :-
	append(L, Acc, L1).
duel([H], [H|T], 1, Acc, [], T2) :-
	append(Acc, [H,H], Tail),
	append(T, Tail, T2).
duel([H|T], [H], 1, Acc, T1, []) :-
	append(Acc, [H,H], Tail),
	append(T, Tail, T1).
duel([H1|T1], [H2|T2], 1, Acc, T1, NT2) :-
	H1 < H2,
	append(Acc, [H1, H2], Tail),
	append(T2, Tail, NT2).
duel([H1|T1], [H2|T2], 1, Acc, NT1, T2) :-
	H1 > H2,
	append(Acc, [H1, H2], Tail),
	append(T1, Tail, NT1).
duel([H,Z1|T1], [H,Z2|T2], N1, Acc, NT1, NT2) :-
	append(Acc, [H, H, Z1, Z2], NewAcc),
	duel(T1, T2, N, NewAcc, NT1, NT2),
	N1 is N + 2.
