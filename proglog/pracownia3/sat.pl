:- op(900,fy,neg).
:- op(1000,yfx,and).
:- op(1010,yfx,or).

instantiate([]).
instantiate([1|T]) :-
  instantiate(T).
instantiate([0|T]) :-
  instantiate(T).

logeval(1).
logeval(X and Y) :-
  logeval(X),
  logeval(Y).
logeval(neg X) :-
  \+ logeval(X).
logeval(X or _) :-
  logeval(X).
logeval(_ or Y) :-
  logeval(Y).

sat(F) :-
  term_variables(F, Vars),
  instantiate(Vars),
  logeval(F).
