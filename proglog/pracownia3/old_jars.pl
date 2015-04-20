zero_list([]).
zero_list([0|T]) :- zero_list(T).

jars(Caps, Target, Result) :-
  length(Caps, Count),
  length(InitialState, Count),
  zero_list(InitialState),
  jars_solution(Caps, Target, [InitialState], 0, SomeResult, 1000),
  % !,
  findBetterSolution(Caps, Target, [InitialState], SomeResult, Result).

findBetterSolution(Caps, Target, States, Limit, Result) :-
  jars_solution(Caps, Target, States, 0, BetterResult, Limit),
  % !,
  findBetterSolution(Caps, Target, States, BetterResult, Result).
findBetterSolution(Caps, Target, States, Limit, Limit).
% findBetterSolution(Caps, Target, States, Limit, Limit) :-
%   \+ jars_solution(Caps, Target, States, 0, _Result, Limit).

% jars_solution(_Caps, _Target, _States, Steps, _Result, Lim) :-
%   Steps >= Lim,
%   !,
%   fail.

jars_solution(_Caps, _Target, [CurrentState|PreviousStates], Steps, _Result, Lim) :-
  (Steps >= Lim; member(CurrentState, PreviousStates)),
  !,
  fail.

jars_solution(Caps, Target, [CurrentState|TailStates], Result, Result, _Lim) :-
  member(Target, CurrentState).
  % add_new_solution(Caps, Target, TailStates, Result),
  % !. % Jesli juz znalezlismy, to nastepne na pewno beda dluzsze.

jars_solution(Caps, Target, [CurrentState|T], Steps, Result, Lim) :-
  \+ member(Target, CurrentState),
  new_step(Caps, CurrentState, NewState),
  NSteps is Steps + 1,
  jars_solution(Caps, Target, [NewState, CurrentState|T], NSteps, Result, Lim).

% add_new_solution(Caps, Target, [HState|TStates], Result) :-
%   add_solution_for(Caps, Target, HState, Result),
%   add_new_solution(Caps, Target, TStates, Result).

% add_solution_for(Caps, Target, HState, Result) :-
%   old_solution(Caps, Target, HState, OldResult),
%   Result < OldResult,
%   asserta((old_solution(Caps, Target, HState, Result) :- !.)).
% add_solution_for(Caps, Target, HState, Result) :-
%   old_solution(Caps, Target, HState, OldResult),
%   Result >= OldResult.

  


% jars_solution(Caps, Target, [CurrentState|T], Steps, Result, Lim) :-
%   make_full(Caps, CurrentState, NewState),
%   NSteps is Steps + 1,
%   jars_solution(Caps, Target, [NewState, CurrentState|T], NSteps, Result, Lim).

% jars_solution(Caps, Target, [CurrentState|T], Steps, Result, Lim) :-
%   make_empty(Caps, CurrentState, NewState),
%   NSteps is Steps + 1,
%   jars_solution(Caps, Target, [NewState, CurrentState|T], NSteps, Result, Lim).

% :- dynamic old_step / 3.

% old_step(Caps, CurrentState, NewState) :- fail.

% step(Caps, CurrentState, NewState) :-
  % old_step(Caps, CurrentState, NewState).
% step(Caps, CurrentState, NewState) :-
%   new_step(Caps, CurrentState, NewState),
  % asserta(old_step(Caps, CurrentState, NewState)).


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



