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

find_identity_2(A):-
  findall(Actor,actor(Actor),Actors), iterate(Actors,A,[]),!.

find_identity_o(A):-
  % Need to generate actor list first.
  % generate_list()...
  my_agent(Agent),
  query_world(agent_current_position,[Agent,P]),
  % Find and memorise stations and oracles
  memorise_stations(Stations,[],P),
  write("Station Position: "), writeln(Stations),
  % Not works for orcles using astar(Stack is exceeded...)
  memorise_oracles(Oracles,[],P),
  write("Oracle Position: "), writeln(Oracles).

  % Need to find identity (agent searches oracles directly).

memorise_stations(Stations,List,P):-
  (length(List,2) -> Stations = List).

memorise_stations(Stations,List,P):-
  solve_task_astar(find(c(C)),[[0,_,P,[P]]],R,Cost,NewPos),
  map_adjacent(NewPos,S_P,c(C)),
  \+ memberchk((S_P,c(C)),List),
  memorise_stations(Stations,[(S_P,c(C))|List],NewPos),!.

memorise_oracles(Oracles,List,P):-
  (length(List,10) -> Oracles = List).

memorise_oracles(Oracles,List,P):-
  solve_task_astar(find(o(O)),[[0,_,P,[P]]],R,Cost,NewPos),
  map_adjacent(NewPos,O_P,o(O)),
  \+ memberchk((O_P,o(O)),List),
  memorise_oracles(Oracles,[(O_P,o(O))|List],NewPos),!.
