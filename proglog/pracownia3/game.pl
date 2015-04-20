% :- dynamic win/2.

% win(L, K) :- fail.

% wygrywa(L, K) :-
%   win(L,K),
%   !.
% wygrywa(L, K) :-
%   max_list(L, Max),
%   Max >= K,
%   assertz((win(L,K))).
% wygrywa(L, K) :-
%   member(MojWybor, L),
%   K1 is K - MojWybor,
%   \+ wygrywa(L, K1),
%   !,
%   assertz((win(L,K))).


:- dynamic wygrywa/2.

wygrywa(L, K) :-
  max_list(L, Max),
  Max >= K,
  asserta((wygrywa(L,K) :- !)).
wygrywa(L, K) :-
  member(MojWybor, L),
  K1 is K - MojWybor,
  \+ wygrywa(L, K1),
  !,
  asserta((wygrywa(L,K) :- !)).



