%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  TESTY DO ZADAŃ SPRAWDZANYCH AUTOMATYCZNIE

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% PREDYKATY STANDARDOWE (uzyte w tekscie)

%  append

%  delete

%  random

%  see

%  seen

%  read

%  get_time

%  write

%  nl











%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Predykaty obliczające prędkość komputera

%



del(X,[X|Xs],Xs).

del(X,[Y|Ys],[Y|Xs]):-

  del(X,Ys,Xs).



makeSum(0,0) :- !.

makeSum(N,1+1*S):- N1 is N-1, makeSum(N1,S).



makeList(0,[]):-!.

makeList(N,[X|Xs]) :- makeSum(N,X), N1 is N-1, makeList(N1,Xs).



range(N,N,[N]).

range(K,N,[K|R]):- K<N, K1 is K+1, range(K1,N,R).



perm([],[]).

perm(X,[E|Ys]):-

  del(E,X,X1),

  perm(X1,Ys).



ordered([_]).

ordered([X,Y|Ys]) :-

  X=<Y, ordered([Y|Ys]).



permsort(L,S):-

  perm(L,S),

  ordered(S).





speedTest(N):-

  makeList(N,L),

  permsort(L,_).





:-dynamic(speed/1).



speed0(V) :-         

  get_time(T1),     

  speedTest(6),

  speedTest(7),

  speedTest(7),

  get_time(T2),

  V is 0.066/(T2-T1+0.001). 



speed(V) :-

  speed0(V1),

  speed0(V2),

  speed0(V3),

  speed0(V4),

  speed0(V5),

  V is (V1+V2+V3+V4+V5)/5.0.









%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Predykaty obliczające wielkość pliku

%



special('!',5).    % Dobrane ad hoc, mogą się zmienić dla dalszych list

special(';',3).    % Mogą tu pojawić się też inne.

special('->',5).

special(':-',0).





termSize(T,1) :- ( var(T); number(T) ), !.

termSize(T,N) :- special(T,N),!.

termSize(T,1) :- atom(T),!.

termSize(T,S) :-

  T =.. [_|Args], argSize(Args,1,S).



argSize([],S,S).

argSize([X|Xs], A, S) :-

  termSize(X,S1),

  A1 is A+S1,

  argSize(Xs,A1,S).



fileSize(File,Size) :-

  concat_atom([File,'.pl'] , FileName),

  see(FileName),

  read(T),

  fileSize(T,0,Size),

  seen.



fileSize(end_of_file, S, S) :-!.

fileSize(T,A,S):-

  termSize(T,TS),

  A1 is A+TS,

  read(NTerm),!,

  fileSize(NTerm,A1,S).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  Predykaty obliczające ocenę







%%%%%%%%%%%%%% DEFINICJA LISTY %%%%%%%%%%%%%%%%%%%



progStylowy(1.0).





pkt(saryt,0.01,0.01,0.05).

pkt(mysort,2,0.01,0.05).

pkt(sum_list,1,0,0.05).

pkt(occur,1,0,0.05).

pkt(permy,0.01,0,0.05).

pkt(term,2,0,0.05).




goodSize(sum_list,29).

goodSize(permy,53).

goodSize(occur,59).

goodSize(term, 150).

goodSize(saryt,120).

goodSize(mysort,85).

goodSize(multiset,79).




goodSpeed(multiset,0.09).



goodSpeed(sum_list,0.00001).

goodSpeed(permy,0.00001).

goodSpeed(occur,0.00001).

goodSpeed(copyterm,0.0000001).



%%%%%%%%%%%%%% KONIEC DEFINICJI LISTY %%%%%%%%%%%%





%%%%%%%%%%%%%% Obliczanie oceny %%%%%%%%%%%%%%%%%%



potega(_,0,1).

potega(K,N,P):- N>0, N1 is N-1, potega(K,N1,P1), P is P1*K.



comp_p(_,K,Y):-potega(2,K,Pot), Y is 1/Pot.

comp_e(Z,T,Y):-

  goodSpeed(Z,G),

  X is G/(T+0.001),

  (X  > 1  -> X1 is X; X1 = X),

  (X1 > 1.5 -> Y=1.5 ; Y=X1).



comp_s(Z,S,Y):-

  goodSize(Z,G),

  X is G/S,

  (X  > 1  -> X1 is X*sqrt(X); X1 = X),

  progStylowy(Prog),

  (X1 > Prog -> Y=Prog ; Y=X1).





ocena(Z,Err,Time,Size,V) :-

  pkt(Z,P,E,S),

  comp_p(Z,Err,MP),  %write(mp(MP)),

  comp_e(Z,Time,ME),  %write(me(ME)),

  comp_s(Z,Size,MS),  %write(me(MS)),

  V is MP*(P+E*ME+S*MS),

  nl,

  write('Zadanie '), write(Z), nl,

  write('  Poprawnosc  '), write(MP),nl,

  write('  Efektywnosc '), write(ME),nl,

  write('  Styl        '), write(MS), nl,

  write('Punkty:  '), write(V),nl.





compute_time(Task,ScaledTime) :-

  prepareTests(Task,Tests),

  speed(Scale),

  te(Tests,_,Time),

  ScaledTime is Time*Scale.



evaluate(L) :- evaluate(L,0,P), write('Ogólna ocena '),write(P), nl.

evaluate([],P,P) :- write('------------- Koniec testów ----------------'), nl.

evaluate([Z|Zs],AP,P) :-

  prepareTests(Z,Testy),

  [Z],

  speed(Scale),

  te(Testy, Errors, Time),

  fileSize(Z,Size), %write(Size),nl,

  ocena(Z,Errors,Time*Scale,Size,PP),  % Wypisuje raport

  AP1 is AP+PP,

  evaluate(Zs,AP1,P).



ev(Z) :-

  prepareTests(Z,Testy),

  [Z], speed(Scale),

  te(Testy,_,Time), T is Time*Scale,

  fileSize(Z,Size),

  write('Czas    '),writeln(T),

  write('Rozmiar '),writeln(Size).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  Przygotowanie testów dla różnych zadań



prepareTests(sum_list,T):-

  T =

  [

    sum_list([0,0,0,0,0,0,0,0,0,0],0),

    sum_list([1,1,1,1,1,1,1,1,1,1],10),

    sum_list([2,2,2,2,2,1,1,1,1,1],15)

  ].



prepareTests(occur,T) :-

  T =

  [

    occurences( 1, 1+2+1+(3*1*7), 3 ),

    occurences( 2, 1+2+1+(3*1*7), 1 ),

    occurences( 0, 1+2+1+(3*1*7), 0 ),

    occurences( 1+2, (1+2)*(1+2)-[1+2,1+2,1+2], 5)

  ].



prepareTests(permy,T) :-

  T =

  [

      ( findall(L1,perm1([1,2,3,4,5],L1),Res1), length(Res1,120) ),

      ( findall(L2,perm2([1,2,3,4,5],L2),Res2), length(Res2,120) ),

      perm1([1,2,3],[3,1,2]),

      perm2([1,2,3],[3,1,2])

  ].



 

prepareTests(saryt,T) :-

  T =

  [

      expTest(2,10,1024),

      expTest(3,0,1),

      minusTest(10,6,4),

      minusTest(10,11,0),

      modTest(12,3,0),

      modTest(201,2,1)

  ].   

     

prepareTests(mysort,T) :-

  largeList(L),

  T =

  [

      sortTest([1,2,3,4,5,6,7,8]),

      sortTest([8,7,6,5,4,3,2,1]),

      sortTest([5,5,5,5,5,5,5,5,5,5,5,5,5]),

      sortTest(L)

  ].

 

prepareTests(multiset,T) :-

  T =

  [

      ( range(1,5000,L), convert(L,M1), list2mset(L,M2), eqmsets(M1,M2)),

      ( range(1,5000,Rq), reverse(Rq,Lq), convert(Lq,M1q), list2mset(Lq,M2q), eqmsets(M1q,M2q)),

      convert([],void),

      ( findall(Xs, convert([1,2,1,1],Xs), Ls), length(Ls,1) ),

      (N=3000,multlist([a,b,c,d],N,Lc),convert(Lc,M1c),eqmsets(M1c, bag(a,N,bag(b,N,bag(c,N,bag(d,N,void))))) ),

      (multlist([a,b,b,b,b],3000,Ld),convert(Ld,M1d),eqmsets(M1d,bag(a,3000,bag(b,12000,void))))

     

  ]. 


prepareTests(term, Tests) :-
  Tests = [
      \+ term([a/0,b/0,c/0], _, 2),
      (findall(T, term([a/0,b/0,c/0], T, 1), Ts), length(Ts,3)),
      (findall(T, term([a/0,b/0,f/5], T, 6), Ts2), length(Ts2,32)),
      \+ term([a/0,b/2], a+a, 3),
      (term([(+)/2, (*)/2, 1/0, 2/0, 3/0], 1+2*3,  R1), R1 = 5),
      (term([(+)/2, (*)/2, 1/0, 2/0, 3/0], (1+2)*3,  R2), R2 = 5)
  ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Mierzenie czasu i sprawdzanie błędów

%



countErrors([],_,N,N).

countErrors([T|Ts],Licznik,N,W) :-

  ( T -> N1=N, write(Licznik - ok),nl ;

        N1 is N+1, write(Licznik - failed),nl,!

  ),

  Licznik1 is Licznik+1,

  countErrors(Ts,Licznik1,N1,W).



te(Testy,Bledy,Czas) :-

  get_time(T1),

  countErrors(Testy,1,0,Bledy),

  get_time(T2),

  Czas0 is T2 - T1,

  (Czas0 = 0 -> Czas = 0.5;

                Czas = Czas0

  ).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  Pierwsza lista: aryt



s2int(0,0).

s2int(s(X),N):- s2int(X,N1),N is N1+1.



int2s(0,0).

int2s(N,s(X)):- N>0, N1 is N-1, int2s(N1,X).



expTest(A,B,C) :-

  int2s(A,As),int2s(B,Bs),

  exp(As,Bs,Cs),

  s2int(Cs,C).

 

minusTest(A,B,C) :-

  int2s(A,As),int2s(B,Bs),

  minus(As,Bs,Cs),

  s2int(Cs,C).

 

modTest(A,B,C) :-

  int2s(A,As),int2s(B,Bs),

  mod(As,Bs,Cs),

  s2int(Cs,C).

 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  Pierwsza lista: sort



appn([],[]).

appn([ [] |Xs], Ys):- appn(Xs,Ys).

appn([ [X|Xs] | Xss], [X|Ys]):- appn( [Xs|Xss], Ys).



largeList(L):-

  L1 = [1,2,3,6,43,7,8,4,3,5,8,4,3,8,9,3,5,7,9,3,4,5,6,7,9,3,4,5,6,7,8,3,12,12,12,34,54,64,3,3,4,5,6,7,7,8,9,89],

  appn([ L1,L1,L1,L1,L1,L1,L1 ], L).



sortTest(L) :-

  mysort(L,Ls),sorted(Ls),length(L,N),length(Ls,N). 



bsortTest(L) :-

  bubsort(L,Ls),sorted(Ls),length(L,N),length(Ls,N). 

 

 

sorted([]).

sorted([_]).

sorted([X,Y|Ys]) :- X=<Y, sorted([Y|Ys]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Druga lista: multizbiory



multlist(L,N,Res) :-

  allthesame(L,N,TS),

  appn(TS,Res).

 

allthesame(_,0,[]) :-!.

allthesame(X,N,[X|Xs]) :-

  N1 is N-1,

  allthesame(X,N1,Xs).

 

mset2list(void,[]).

mset2list(bag(A,B,M), [ (A,B) | L] ):-

  mset2list(M,L).

 

list2mset([],void).

list2mset([X|Xs],bag(X,1,M)) :-

  list2mset(Xs,M). 

 

eqmsets(A,B) :-

  mset2list(A,AL),

  mset2list(B,BL),

  msort(AL,Sorted),

  msort(BL,Sorted).

 





%-------------------------------------------------



:- write(' Wersja testow z 4 marca 2015'), nl.

:- write(' W kartotece, w ktorej uruchamia sie program powinny znajdowac sie pliki '), nl.

:- write('    saryt.pl,  permy.pl (za prawie zero)'), nl.
:- write('    sum_list.pl, occur.pl, mysort.pl, multiset.pl, term'), nl.



:- write(' Uruchamianie: evaluate(lista nazw rozwiazanych zadan)'),nl.

:- write('  np: evaluate([permy, sum_list])'),nl.



%:-write(' Ostatnie zmiany: Informacja na stronie wykładu'),nl.
