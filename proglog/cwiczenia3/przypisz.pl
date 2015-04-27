:- dynamic baza_zmiennych/2.

przypisz(Var, Value) :-
  asserta((baza_zmiennych(Var, Value) :- !)).
przypisz(Var, Value) :-
  retract((baza_zmiennych(Var, Value) :- !)),
  fail.

pobierz(Var, Value) :-
  var(Value),
  baza_zmiennych(Var, Value).

wyswietlx(Tag, Var) :-
  write('Wyswietlam `'),
  write(Tag),
  write('`: '),
  write(Var),
  nl.

testprzypisz :-
  przypisz(foo, 7),
  pobierz(foo, X),
  wyswietlx(1, X),
  pred1.

pred1 :-
  przypisz(foo, 5),
  pobierz(foo, Y),
  wyswietlx(2, Y),
  pobierz(foo, 7).

pred1 :-
  pobierz(foo, Z),
  Z = 7,
  wyswietlx(3, Z).
