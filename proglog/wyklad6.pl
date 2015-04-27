?- use_module(library(clpfd)).

solve(Vars) :-
  Vars = [C,I,A,H,O,N,D,W,G],
  Vars ins 0..9,
  200000*C + 20000*I + 2000*A + 200*C + 20*H + 2*O #= 1000000*N + 100000*A + 10000*D + 1000*W + 100*A + 10*G + A,
  all_different(Vars),
  C #\= 0, N #\= 0,
  label(Vars).
