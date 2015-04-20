zero_list([]).
zero_list([0|T]) :- zero_list(T).

jars(Caps, Target, Result) :-
  length(Caps, Count),
  length(InitialState, Count),
  zero_list(InitialState),
  jars_solution(Caps, Target, [InitialState], [], 0, Result).

jars_solution(Caps, Target, States, OldStates, Result, Result) :-
  append(States, FStates),
  member(Target, FStates).

jars_solution(Caps, Target, [], OldStates, Count, Result) :-
  !,
  fail.

jars_solution(Caps, Target, States, OldStates, Count, Result) :-
  findallnewstates(Caps, States, AllNewStates),
  append(AllNewStates, NewStates),
  sort(NewStates, UniqNewStates),
  succ(Count, NewCount),
  append(OldStates, States, OldStates2),
  sort(OldStates2, OldStates3),
  remove_dups(UniqNewStates, OldStates3, UniqNewStates2),
  jars_solution(Caps, Target, UniqNewStates2, OldStates3, NewCount, Result).

findallnewstates(_Caps, [], []).
findallnewstates(Caps, [H|T], [NewStates|TNewStates]) :-
  findall(State2, new_step(Caps, H, State2), NewStates),
  findallnewstates(Caps, T, TNewStates).

remove_dups([], _, []).
remove_dups([H|T], OldStates, T1) :-
  member(H, OldStates),
  remove_dups(T, OldStates, T1).
remove_dups([H|T], OldStates, [H|T1]) :-
  \+ member(H, OldStates),
  remove_dups(T, OldStates, T1).

new_step(Caps, CurrentState, NewState) :-
  make_full(Caps, CurrentState, NewState).
new_step(Caps, CurrentState, NewState) :-
  make_empty(Caps, CurrentState, NewState).
new_step(Caps, CurrentState, NewState) :-
  move(Caps, CurrentState, NewState).

make_full([Cap|_TCaps], [_Val|TVals], [Cap|TVals]).
make_full([_|TCaps], [Val|TVals], [Val|NTVals]) :-
  make_full(TCaps, TVals, NTVals).

make_empty(_, [_Val|TVals], [0|TVals]).
make_empty(Caps, [Val|TVals], [Val|NTVals]) :-
  make_empty(Caps, TVals, NTVals).

move2(ValFrom, ValTo, CapTo, ValFrom2, ValTo2) :-
  DiffTo is CapTo - ValTo,
  DiffTo >= ValFrom,
  ValFrom2 = 0,
  ValTo2 is ValTo + ValFrom.

move2(ValFrom, ValTo, CapTo, ValFrom2, ValTo2) :-
  DiffTo is CapTo - ValTo,
  DiffTo < ValFrom,
  ValFrom2 is ValFrom - DiffTo,
  ValTo2 = CapTo.

set_value(1, [_|TVals], NewVal, [NewVal|TVals]) :- !.
set_value(Position, [Val|TVals], NewVal, [Val|TVals2]) :-
  succ(NPosition, Position),
  set_value(NPosition, TVals, NewVal, TVals2).

move(Caps, CurrentState, NewState) :-
  length(Caps, L),
  between(1, L, JarFrom),
  between(1, L, JarTo),
  JarFrom =\= JarTo,
  nth1(JarFrom, CurrentState, ValFrom),
  ValFrom =\= 0,
  nth1(JarTo, CurrentState, ValTo),
  nth1(JarTo, Caps, CapTo),
  ValTo =\= CapTo,
  move2(ValFrom, ValTo, CapTo, ValFrom2, ValTo2),
  set_value(JarFrom, CurrentState, ValFrom2, CurrentState2),
  set_value(JarTo, CurrentState2, ValTo2, NewState).
