edge(wroclaw, lodz). 
edge(lodz, warszawa). 
edge(lodz, siedlce). 
edge(warszawa, biala_podl). 
edge(warszawa, krakow). 
edge(krakow, wroclaw). 
edge(krakow, lodz). 
edge(siedlce, biala_podl). 
edge(siedlce, warszawa).

connected(X, Y) :-
  connected2(X, Y, [X], []).

connected2(Source, Target, States, OldVisited) :-


