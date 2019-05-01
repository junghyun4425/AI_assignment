% candidate_number(12345).

% Find hidden identity by repeatedly calling agent_ask_oracle(oscar,o(1),link,L)
% find_identity(-A)
find_identity(A):-
  (part_module(2)   -> find_identity_2(A)
  ; otherwise -> find_identity_o(A)
  ).

possible(Links,A,Actors) :- member(A,Actors), wp:actor_links(A,L),
                            forall(member(Link,Links),member(Link,L)).

iterate([A],A,_).
iterate(Actors,A,Links) :-
      agent_ask_oracle(oscar,o(1),link,L),
      (not(member(L,Links)) -> NewLinks = [L|Links], findall(A,possible(NewLinks,A,Actors),NewActors)
      ; otherwise -> NewLinks = Links, NewActors = Actors),
      iterate(NewActors,A,NewLinks).

iterate_o([A],A,_,_).
iterate_o(Actors,A,Links,I) :-
          solve_task(find(o(I)),_Cost),
          game_predicates:agent_ask_oracle(oscar,o(I),link,L),
          (not(member(L,Links)) -> NewLinks = [L|Links], findall(A,possible(NewLinks,A,Actors),NewActors)
          ; otherwise -> NewLinks = Links, NewActors = Actors),
          nI is I + 1, iterate_o(NewActors,A,NewLinks,nI).


find_identity_2(A):-
  findall(Actor,actor(Actor),Actors), iterate(Actors,A,[]),!.

find_identity_o(A):-
  findall(Actor,actor(Actor),Actors), iterate_o(Actors,A,[],1),!.
