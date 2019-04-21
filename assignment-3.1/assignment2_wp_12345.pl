% candidate_number(12345).

% Find hidden identity by repeatedly calling agent_ask_oracle(oscar,o(1),link,L)
% find_identity(-A)
find_identity(A):-
  (part_module(2)   -> find_identity_2(A)
  ; otherwise -> find_identity_o(A)
  ).

possible(Links,A) :- actor(A), wp:actor_links(A,L), forall(member(Link,Links),member(Link,L)).

iterate([A],A,_).
iterate(_,A,Links) :-
      agent_ask_oracle(oscar,o(1),link,L),
      (\+member(L,Links) -> append([L],Links,NewLinks) ; otherwise -> NewLinks = Links),
      findall(Actor,possible(NewLinks,Actor),NewActors), iterate(NewActors,A,NewLinks).

find_identity_2(A):-
  findall(Actor,actor(Actor),Actors), iterate(Actors,A,[]),!.

find_identity_o(A):-
  A='Not yet implemented'.
