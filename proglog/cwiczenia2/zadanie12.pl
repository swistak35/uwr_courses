lookup(Key, Dict, Value) :-
  Dict = dict(Key, Value2, Left, Right),
  !,
  Value = Value2.

lookup(Key, Dict, Value) :-
  Dict = dict(Key1, _Value, Left, _Right),
  Key =< Key1,
  lookup(Key, Left, Value).

lookup(Key, Dict, Value) :-
  Dict = dict(Key1, _Value, _Left, Right),
  Key >= Key1,
  lookup(Key, Right, Value).

assertp(Pred) :-
  assertz(Pred).
assertn(Pred) :-
  asserta((Pred :- !, fail)).

