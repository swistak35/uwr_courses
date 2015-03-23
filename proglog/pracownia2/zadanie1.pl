edge(wroclaw, lodz). 
edge(lodz, warszawa). 
edge(lodz, siedlce). 
edge(warszawa, biala_podl). 
edge(warszawa, krakow). 
edge(krakow, wroclaw). 
edge(krakow, lodz). 
edge(siedlce, biala_podl). 
edge(siedlce, warszawa).

connected(Source, Destination):-
  connected3(Source, Destination, [Destination]).

connected3(Source, Destination, _Visited):-
  edge(Source, Destination).

connected3(Source, Destination, Visited):-
  edge(Current, Destination),
  \+ member(Current, Visited),
  connected3(Source, Current, [Current|Visited]).
