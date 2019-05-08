candidate_number(12345).

% find hidden identity
find_identity(A):-
  (part_module(2)   -> find_identity_2(A)
  ; otherwise -> find_identity_o(A)
  ).

% true if actor A has all Links (if Links is subset of the links of that actor)
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

% If only one actor is possible, we are finished
iterate_o([A],A,_,_).

% Find an oracle that has not been visited yet, query it, add the link, and proceed as in iterate
iterate_o(_Actors,A,Links,Visited) :-
                          my_agent(Agent),
                          solve_task(find(o(I)),_,Visited),
                          query_world(agent_ask_oracle, [Agent,o(I),link,L]),
                          (\+member(L,Links) -> append([L],Links,NewLinks) ; otherwise -> NewLinks = Links),
                          findall(Actor,possible(NewLinks,Actor),NewActors),
                          iterate_o(NewActors,A,NewLinks,[o(I)|Visited]).

% Initialise iterations to find identity
find_identity_o(A):-
  findall(Actor,actor(Actor),Actors),
  iterate_o(Actors,A,[],[]).
