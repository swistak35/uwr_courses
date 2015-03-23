factSize(Fact, Size) :-
  Fact =.. [_Name | Args],
  maplist(factSize, Args, Results),
  sum_list([1|Results], Size).

sizeOfConclusionsLessThan([], _MFL).
sizeOfConclusionsLessThan([H|T], MFL) :-
  factSize(H, Size),
  Size =< MFL,
  sizeOfConclusionsLessThan(T, MFL).

conclusionsAlreadyInFacts(_Facts, []).
conclusionsAlreadyInFacts(Facts, [Conclusion|TailConclusions]) :-
  member(Conclusion, Facts),
  conclusionsAlreadyInFacts(Facts, TailConclusions).

matchFacts(_Facts, []).
matchFacts(Facts, [HF|TF]) :-
  member(HF, Facts),
  matchFacts(Facts, TF).

tryToApplyRule(Facts, Rule, MFL, Conclusions) :-
  copy_term(Rule, FreshRule),
  RequiredFacts >> Conclusions = FreshRule,
  matchFacts(Facts, RequiredFacts),
  sizeOfConclusionsLessThan(Conclusions, MFL),
  \+ conclusionsAlreadyInFacts(Facts, Conclusions).

tryToApplySomeRule(_Facts, [], _MFL, _Result) :-
  fail.
tryToApplySomeRule(Facts, [Rule|_TailRules], MFL, Result) :-
  tryToApplyRule(Facts, Rule, MFL, Result).
tryToApplySomeRule(Facts, [_Rule|TailRules], MFL, Result) :-
  tryToApplySomeRule(Facts, TailRules, MFL, Result).

inference(Facts, Rules, MaximumFaktLength, Result) :-
  tryToApplySomeRule(Facts, Rules, MaximumFaktLength, Conclusions),
  !,
  append(Facts, Conclusions, NewFacts),
  inference(NewFacts, Rules, MaximumFaktLength, Result).
inference(Facts, _Rules, _MaximumFaktLength, Facts).
