candidate_number(12345).

% find hidden identity
find_identity(A):-
  (part_module(2)   -> find_identity_2(A)
  ; otherwise -> find_identity_o(A)
  ).

% all links associated to an actor
possible(Links,A) :- actor(A), wp:actor_links(A,L), forall(member(Link,Links),member(Link,L)).

% finds secret identity with the links provided by comparing and eliminating
iterate([A],A,_).
iterate(_,A,Links) :-
      agent_ask_oracle(oscar,o(1),link,L),
      (\+member(L,Links) -> NewLinks = [L|Links] ; otherwise -> NewLinks = Links),
      findall(Actor,possible(NewLinks,Actor),NewActors), iterate(NewActors,A,NewLinks).

% deduces the secret identity
find_identity_2(A):-
  findall(Actor,actor(Actor),Actors), iterate(Actors,A,[]),!.

% agent goes around the grid world, querying once each oracle providing links to deduce its secret identity
iterate_o([A],A,_,_,_).
iterate_o(Actors,A,Links,N,Visited) :-
                          my_agent(Agent),
                          query_world( agent_current_position, [Agent,P] ),
                          N1 is N + 1,
                          solve_task(find(o(I)),_,Visited),
                          query_world(agent_ask_oracle, [Agent,o(I),link,L]),
                          (\+member(L,Links) -> append([L],Links,NewLinks) ; otherwise -> NewLinks = Links),
                          findall(Actor,possible(NewLinks,Actor),NewActors),
                          iterate_o(NewActors,A,NewLinks,N1,[o(I)|Visited]).

% finds hidden identity by going through the list of possible actors 
find_identity_o(A):-
  findall(Actor,actor(Actor),Actors),
  iterate_o(Actors,A,[],0,[]).
