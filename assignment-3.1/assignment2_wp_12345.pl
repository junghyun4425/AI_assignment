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
      (\+member(L,Links) -> NewLinks = [L|Links] ; otherwise -> NewLinks = Links),
      findall(Actor,possible(NewLinks,Actor),NewActors), iterate(NewActors,A,NewLinks).

iterate_o([A],A,_,_).
iterate_o(Actors,A,Links,I) :-
          solve_task(find(o(I)),_Cost),
          game_predicates:agent_ask_oracle(oscar,o(I),link,L),
          (not(member(L,Links)) -> NewLinks = [L|Links], findall(A,possible(NewLinks,A,Actors),NewActors)
          ; otherwise -> NewLinks = Links, NewActors = Actors),
          nI is I + 1, iterate_o(NewActors,A,NewLinks,nI).


find_identity_2(A):-
  findall(Actor,actor(Actor),Actors), iterate(Actors,A,[]),!.

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

find_identity_o(A):-
  findall(Actor,actor(Actor),Actors),
  iterate_o(Actors,A,[],0,[]).
