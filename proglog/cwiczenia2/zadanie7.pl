findall2(Template, Goal, Bag, Acc) :-
  \+ \+ Goal,
  findall(Template, Goal, Bag, [])
