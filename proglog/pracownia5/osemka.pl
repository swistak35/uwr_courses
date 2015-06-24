osemka(InitialState, Target, N) :-
  osemka_solution(Target, [InitialState], [], 0, N).

osemka_solution(Target, States, _OldStates, Result, Result) :-
  member(Target, States),
  !.

osemka_solution(_Target, [], _OldStates, _Count, _Result) :-
  !,
  fail.

osemka_solution(Target, States, OldStates, Count, Result) :-
  findallnewstates(States, AllNewStates),
  append(AllNewStates, NewStates),
  sort(NewStates, UniqNewStates),
  succ(Count, NewCount),
  append(OldStates, States, OldStates2),
  sort(OldStates2, OldStates3),
  remove_dups(UniqNewStates, OldStates3, UniqNewStates2),
  osemka_solution(Target, UniqNewStates2, OldStates3, NewCount, Result).

findallnewstates([], []).
findallnewstates([H|T], [NewStates|TNewStates]) :-
  findall(State2, new_step(H, State2), NewStates),
  findallnewstates(T, TNewStates).

remove_dups([], _, []).
remove_dups([H|T], OldStates, T1) :-
  member(H, OldStates),
  remove_dups(T, OldStates, T1).
remove_dups([H|T], OldStates, [H|T1]) :-
  \+ member(H, OldStates),
  remove_dups(T, OldStates, T1).

new_step(State, NextState) :-
  nth1(Pos, State, o),
  next_step(Pos, State, NextState).

next_step(Pos, State, NextState) :-
  Pos mod 3 =\= 0,
  move_right(State, NextState).
next_step(Pos, State, NextState) :-
  Pos mod 3 =\= 1,
  move_left(State, NextState).
next_step(Pos, State, NextState) :-
  Pos > 3,
  move_up(State, NextState).
next_step(Pos, State, NextState) :-
  Pos < 7,
  move_down(State, NextState).

move_left(State, NewState) :-
  append([Prefix, [Y, o], Suffix], State),
  append([Prefix, [o, Y], Suffix], NewState).
move_right(State, NewState) :-
  append([Prefix, [o, Y], Suffix], State),
  append([Prefix, [Y, o], Suffix], NewState).
move_up(State, NewState) :-
  append([Prefix, [X1, X2, X3, o], Suffix], State),
  append([Prefix, [o, X2, X3, X1], Suffix], NewState).
move_down(State, NewState) :-
  append([Prefix, [o, X1, X2, X3], Suffix], State),
  append([Prefix, [X3, X1, X2, o], Suffix], NewState).
