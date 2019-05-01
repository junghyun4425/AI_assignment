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
  my_agent(Agent),
  query_world(agent_current_position,[Agent,P]),
  % Find and memorise stations and oracles
  memorise_stations(Stations,[],P),
  write("Stations: "), writeln(Stations),
  memorise_oracles(Oracles,[],P),
  write("Oracles: "), writeln(Oracles),
  % Need to generate actor list first.
  % generate_list()...
  checkout_oracles(A,Agent,Actors,Stations,Oracles,[]).

% memorise stations & oracles
% description: Find all of stations or oracles using astar algorithm
% input: current position P, empty List
% output: list of stations or oracles
memorise_stations(Stations,List,_):-
  (length(List,2) -> Stations = List).

memorise_stations(Stations,List,P):-
  solve_task_astar(find(c(C)),[[0,_,P,[P]]],_,_,NewPos),
  map_adjacent(NewPos,S_P,c(C)),
  \+ memberchk((S_P,c(C)),List),
  memorise_stations(Stations,[(S_P,c(C))|List],NewPos),!.

memorise_oracles(Oracles,List,_):-
  (length(List,10) -> Oracles = List).

memorise_oracles(Oracles,List,P):-
  solve_task_astar(find(o(O)),[[0,_,P,[P]]],_,_,NewPos),
  map_adjacent(NewPos,O_P,o(O)),
  \+ memberchk((O_P,o(O)),List),
  memorise_oracles(Oracles,[(O_P,o(O))|List],NewPos),!.

% checkout_oracles
% description: This will drop by all of oracles. Find identity or somtimes fail.
% input: A, Agent, list of actors, list of stations, list of oracles, closedset
% output: print fail or success, moving to oracles or stations
checkout_oracles(A,_,Actors,_,_,_):-
  (Actors = [[ actor(A)|_]] -> true). % Found

checkout_oracles(_,_,_,_,_,ClosedSet):-
  (length(ClosedSet,10) -> false). % Not Found

checkout_oracles(A,Agent,Actors,Stations,Oracles,ClosedSet):-
  query_world(agent_current_energy,[Agent,Energy]),
  query_world(agent_current_position,[Agent,P]),
  write("Energy = "), writeln(Energy),
  write("Position = "), writeln(P),
  find_oracle_cost(NextOracle,P,Oracles,ClosedSet).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  % 1.Make cost list
  % 2.Find lowest cost of oracle (do this: (\+ memberchk(O or C, ClosedSet) -> make goal)
  %    Above give us postion of oracle (our goal)
  % 3.Compare lowest cost vs remaining energy
  % 4.If big enough
  %   - Use astar algorithm to go to the goal
  % 4.If not
  %   - Find nearest station
  % 5. move_to_path making??
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% find oracle & station cost
% description: find oracle or station, which has lowest cost.
% input: current position P, list of objects(location of oracles or stations)
% output: next station or oracle (lowest cost)
find_station_cost(NextStation,P,Stations):-
  get_costs(P,Stations,Costs,[]),
  sort(Costs,SortedCosts),
  SortedCosts = [(_,Pos,CID)|_],
  NextStation = (Pos,CID).

find_oracle_cost(NextOracle,P,Oracles,ClosedSet):-
  get_costs(P,Oracles,Costs,[]),
  sort(Costs,SortedCosts),
  check_closedset(Goal,SortedCosts,ClosedSet),
  Goal = (_,Pos,OID),
  NextOracle = (Pos,OID).

% get_costs
% description: Get all of cost from current position to objects (object: orcles, stations)
% input: current position P, object list
% output: list of cost [(cost, object_location)|...]
get_costs(_,[],Costs,List):-
  Costs = List.

get_costs(P,Objects,Costs,List):-
  Objects =[(Pos,ID)|Rest],
  map_distance(P,Pos,Cost),
  append([(Cost,Pos,ID)],List,NewList),
  get_costs(P,Rest,Costs,NewList),!.

% check_closedset
% description: check oracles whether it visited or not.
% input: list of cost (should be sorted), list of closedset
% output: next oracle (did not visit)
check_closedset(Goal,SortedCosts,ClosedSet):-
  SortedCosts = [(Cost,Pos,OID)|Rest],
  (\+ memberchk(OID, ClosedSet) -> Goal = (Cost,Pos,OID)
  ; check_closedset(Goal, Rest, ClosedSet)).

% update_closedset
% description: update closedset if the OID was visited
% input: OID (o(O)), list of closedset
% output: closedset added OID (NewClosedSet)
update_closedset(NewClosedSet, OID, ClosedSet):-
  append(OID,ClosedSet,Appended),
  NewClosedSet = Appended.

% move_to_go
% description: move to do task.
% input: Task (mainly go), Agent, current position P
% output: move agent on webpage
move_to_go(Task,Agent,P):-
  solve_task_astar(Task,[[0,_,P,[P]]],R,_,_NewPos),!,
  reverse(R,[_Init|Path]),
  query_world( agent_do_moves, [Agent,Path] ).
