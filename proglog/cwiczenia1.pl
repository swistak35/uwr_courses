% Zadanie 4
reverse2(X, Y) :-
  reverse3(X, Y, []).

reverse3([], Y, Y).
reverse3([X|Xs], Y, Acc) :-
  append([X], Acc, NewAcc),
  reverse3(Xs, Y, NewAcc).

% Zad 5

prefix([], _).
prefix([X|Xs], [X|Ys]) :- prefix(Xs, Ys).
suffix(Xs, Xs).
suffix(Xs, [_|Ys]) :- suffix(Xs, Ys).

sublist(X, Y) :-
  prefix(P, Y),
  suffix(X, P).

member2(X, L) :-
  sublist([X], L).

prefix2(X, Y) :-
  append(X, _, Y).

suffix2(X, Y) :-
  append(_, X, Y).

adjacent(X, Y, L4) :-
  append(L2, L3, L4),
  append(L1, [X, Y], L2).
adjacent(X, Y, L4) :-
  append(L2, L3, L4),
  append(L1, [Y, X], L2).

last(X, L) :-
  append(_, [X], L).

% Zad 6
writeAllLisp([]).
writeAllLisp([X]) :-
  writeLisp(X).
writeAllLisp([H,H1|T]) :-
  writeLisp(H),
  write(' '),
  writeAllLisp([H1|T]).

writeLisp(X) :-
  compound(X),
  !,
  X =.. L,
  write('('),
  writeAllLisp(L),
  write(')').

writeLisp(X) :-
  write(X).
  
% Zad 7

shuffle_term(X, Y) :-
  X =.. [H|T],
  shuffle_list(T, T1),
  Y =.. [H|T1].
shuffle_term(X, Y) :-
  X =.. [H|Args],
  shuffle_subterm(Args, Args2),
  Y =.. [H|Args2].

shuffle_subterm([H|T], [H1|T]) :-
  shuffle_term(H, H1).
shuffle_subterm([H|T], [H|T1]) :-
  shuffle_subterm(T, T1).


shuffle_list([H|T], [H1|T1]) :-
  choose_another(H, T, H1, T1).
shuffle_list([H|T], [H|T1]) :-
  shuffle_list(T, T1).

choose_another(H, [X|Xs], X, [H|Xs]).
choose_another(H, [X|Xs], Y, [X|Ys]) :-
  choose_another(H, Xs, Y, Ys).


  
