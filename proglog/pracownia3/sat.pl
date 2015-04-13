:- op(900,fy,neg).
:- op(1000,yfx,and).
:- op(1010,yfx,or).

sat(F) :-
  term_variables(F, Vars),
  instantiate(Vars),
  evaluate(F, 1).

instantiate([]).
instantiate([1|T]) :-
  instantiate(T).
instantiate([0|T]) :-
  instantiate(T).

  
evaluate(1, 1).
evaluate(0, 0).

evaluate(neg X, 1) :-
  evaluate(X, 0).
evaluate(neg X, 0) :-
  evaluate(X, 1).

evaluate(X1 and X2, 1) :-
  evaluate(X1, 1),
  evaluate(X2, 1).
evaluate(X1 and _, 0) :-
  evaluate(X1, 0).
evaluate(_ and X2, 0) :-
  evaluate(X2, 0).

evaluate(X1 or _, 1) :-
  evaluate(X1, 1).
evaluate(_ or X2, 1) :-
  evaluate(X2, 1).
evaluate(X1 or X2, 0) :-
  evaluate(X1, 0),
  evaluate(X2, 0).
