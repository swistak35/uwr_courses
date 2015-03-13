%zad0a - exp

plus(0,X,X).
plus(s(X),Y,s(Z)) :-
	plus(X,Y,Z).

% nie dziala (-,-,+)
mult(0,_,0).
mult(s(X),Y,Z) :-
	plus(Y,Z1,Z),
	mult(X,Y,Z1).
	
%exp(0,X,0) :-
%	!, X \= 0.
exp(_,0,s(0)).
exp(X,s(Y),Z) :-
	exp(X,Y,Z1),
	mult(X,Z1,Z).
	
%zad0b - minus

% nie dziala (+,-,+)
minus(X,0,X).
minus(X,s(Y),0) :-
	minus(X,Y,0).
minus(X,s(Y),Z) :-
	minus(X,Y,s(Z)).

%zad0c - mod

mod(0,_,0).
mod(s(X),Y,0) :-
	minus(s(X),Y,0),
	s(X) = Y.
mod(s(X),Y,s(X)) :-
	minus(s(X),Y,0),
	s(X) \= Y.
mod(s(X),Y,Z) :-
	minus(s(X),Y,s(X1)),
	mod(s(X1),Y,Z).
