checkArity([H|_], Name, Arity) :-
  H = Name/Arity.
checkArity([_|T], Name, Arity) :-
  checkArity(T, Name, Arity).

argsWithGoodSize(Sig, [], 0).
argsWithGoodSize(Sig, [H|T], ArgsSize) :-
  ArgsSize >= 0,
  between(0, ArgsSize, Size),
  term2(Sig, Size, H),
  RestSize is ArgsSize - Size,
  argsWithGoodSize(Sig, T, RestSize).

term2(Sig, Size, Term) :-
  nonvar(Sig),
  succ(ArgsSize, Size),
  checkArity(Sig, Name, Arity),
  functor(Term, Name, Arity),
  Term =.. [Name|Args],
  argsWithGoodSize(Sig, Args, ArgsSize).

term(Sig, Size, Term) :-
  nonvar(Size),
  term2(Sig, Size, Term).

term(Sig, Size, Term) :-
  nonvar(Term),
  length(_, Size),
  term2(Sig, Size, Term),
  !.
