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

allArgsComplyToSig([], Sig, Size, Size) :- !.
allArgsComplyToSig([H|T], Sig, Acc, ResultSize) :-
  term(Sig, Size, H),
  NewAcc is Acc + Size,
  allArgsComplyToSig(T, Sig, NewAcc, ResultSize).

% term(Sig, Size, Term) :-
%   checkArity(Sig, Name, Arity),
%   functor(Term, Name, Size),
%   Term =.. [Name|Args],
%   allArgsComplyToSig(Args, Sig).

% term2(Sig, Size, Term) :-
%   sortAndConvert(Sig, Sig2),
%   term(Sig2, Size, Term).

term(Sig, Size, Term) :-
  nonvar(Sig),
  nonvar(Term),
  checkArity(Sig, Name, Arity),
  functor(Term, Name, Arity),
  Term =.. [Name|Args],
  allArgsComplyToSig(Args, Sig, 1, Size).

makeAllArgsComply(Sig, [], Size, ResultSize).
makeAllArgsComply(Sig, [H|T], NewSize, ResultSize) :-
  mkTerm(Sig, NewSize, H, NextSize),
  % NextSize is NewSize - Size,
  makeAllArgsComply(Sig, T, NextSize, ResultSize).


mkTerm(Sig, Size, Term, ResultSize) :-
  nonvar(Sig),
  nonvar(Size),
  Size >= 0,
  checkArity(Sig, Name, Arity),
  functor(Term, Name, Arity),
  Term =.. [Name|Args],
  NewSize is Size - 1,
  makeAllArgsComply(Sig, Args, NewSize, ResultSize).

term(Sig, Size, Term) :-
  nonvar(Sig),
  nonvar(Size),
  mkTerm(Sig, Size, Term, ResSize),
  ResSize = 0.
