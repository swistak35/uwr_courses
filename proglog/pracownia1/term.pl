checkArity([H|_], Name, Arity) :-
  H = Name/Arity.
  % !.
checkArity([_|T], Name, Arity) :-
  checkArity(T, Name, Arity).

convertList([], []).
convertList([H1|T1], [H2|T2]) :-
  H1 = X/Y,
  H2 = Y-X,
  convertList(T1,T2).

sortAndConvert(L1, L1Sorted) :-
  convertList(L1, L2),
  keysort(L2, L2Sorted),
  convertList(L1Sorted, L2Sorted).

allArgsComplyToSig([], Sig) :- !.
allArgsComplyToSig([H|T], Sig) :-
  term(Sig, S, H),
  allArgsComplyToSig(T, Sig).

term(Sig, Size, Term) :-
  checkArity(Sig, Name, Size),
  functor(Term, Name, Size),
  Term =.. [Name|Args],
  allArgsComplyToSig(Args, Sig).

term2(Sig, Size, Term) :-
  sortAndConvert(Sig, Sig2),
  term(Sig2, Size, Term).
