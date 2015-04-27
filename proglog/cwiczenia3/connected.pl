edge(wroclaw, lodz). 
edge(lodz, warszawa). 
edge(lodz, siedlce). 
edge(warszawa, biala_podl). 
edge(warszawa, krakow). 
edge(krakow, wroclaw). 
edge(krakow, lodz). 
edge(siedlce, biala_podl). 
edge(siedlce, warszawa).

revedge(X, Y) :- edge(Y, X).

connected(X, Y) :-
  connected2(Y, [X], []).

remove([], _, []).
remove([H|T], OldStates, T1) :-
  member(H, OldStates),
  remove(T, OldStates, T1).
remove([H|T], OldStates, [H|T1]) :-
  \+ member(H, OldStates),
  remove(T, OldStates, T1).

findalltargets(Source, Targets) :-
  findall(Target, edge(Source, Target), Targets).

connected2(Target, States, _OldVisited) :-
  member(Target, States),
  !.

connected2(_Target, [], _OldVisited) :-
  !,
  fail.

connected2(Target, States, OldVisited) :-
  maplist(findalltargets, States, AllNewStates),
  append(AllNewStates, AllNewStates2),
  sort(AllNewStates2, UniqNewStates),
  append(OldVisited, States, NewOldVisited),
  remove(UniqNewStates, NewOldVisited, UniqNewStates2),
  connected2(Target, UniqNewStates2, NewOldVisited).


