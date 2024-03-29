candidate_number(12345).

% current energy of the agent
get_energy(E) :- my_agent(Agent),query_world( agent_current_energy, [Agent,E] ).

% agent tops up energy
recharge :-   my_agent(Agent),
              query_world( agent_current_position, [Agent,P] ),
              solve_task_astar(find(c(I)),[[0,_,P,[P]]],R,_Cost,_NewPos,[]),!,  % find a charging station
              reverse(R,[_Init|Path]),
              query_world( agent_do_moves, [Agent,Path] ),    % go to charging station
              query_world( agent_topup_energy, [Agent,c(I)] ).  % top up

% agent solves task using an agenda based search, and tops up if energy after move would be lower than 40
solve_task(Task,Cost):-
  my_agent(Agent),
  repeat,
  query_world( agent_current_position, [Agent,P] ),
  solve_task_astar(Task,[[0,_,P,[P]]],R,Cost,_NewPos,[]),!,  % prune choice point for efficiency
  get_energy(Energy), Cost = [cost(C)|_], Threshold = 40,
  (Energy - C < Threshold ->  recharge,
                          solve_task(Task,_Cost)
  ;
  otherwise ->  reverse(R,[_Init|Path]),
                query_world( agent_do_moves, [Agent,Path])
  ).

% agent solves task using an agenda based search, makes sure it finds a solution that is not banned
% (for part 4), and if the energy left after making the move is too low, it tops up before making it
solve_task(Task,Cost,Banned):-
  my_agent(Agent),
  repeat,
  query_world( agent_current_position, [Agent,P] ),
  solve_task_astar(Task,[[0,_,P,[P]]],R,Cost,_NewPos,Banned),!,  % prune choice point for efficiency
  get_energy(Energy), Cost = [cost(C)|_], Threshold = 40,
  (Energy - C < Threshold ->  recharge,
                          solve_task(Task,_Cost)
  ;
  otherwise ->  reverse(R,[_Init|Path]),
                query_world( agent_do_moves, [Agent,Path] )
  ).

% children are added to an agenda, making sure those positions have not been reached already
children([G,_,P,RPath],[G1,H1,P1,[P1|RPath]],Task,Agenda) :- search(P,P1,_,_), G1 is G + 1,
                                                      heuristics(P1,H1,Task),
                                                      not(member([_,_,P1,_],Agenda)).

% gives an estimate if the location of the goal is known, if not, H-value is 0
heuristics(P,H1,go(Exit)) :- map_distance(P,Exit,H1).
heuristics(_,0,find(_)).

% looks for the element with lowest F value, and places it first on the list
find_min([H|T],Result) :- find_min_aux(H,[],T,Result).
find_min_aux(Smallest,Seen,[],[Smallest|Seen]).
find_min_aux(Smallest,Seen,[H|T],Result) :- lt(H,Smallest),!,find_min_aux(H,[Smallest|Seen],T,Result) ; find_min_aux(Smallest,[H|Seen],T,Result).
lt([G1,H1,_,_],[G2,H2,_,_]) :- F1 is G1 + H1, F2 is G2 + H2, F1 < F2.

% using an A*search to find the lowest cost path to the goal
% base case: the node with lowest F value is a solution
solve_task_astar(Task,Agenda,RPath,[cost(Cost),depth(Cost)],NewPos,Banned) :-
  find_min(Agenda,TopFirst),
  achieved(Task,TopFirst,RPath,Cost,NewPos,Banned).

% recursive case: add children of node with lowest F value
solve_task_astar(Task,Agenda,RR,Cost,NewPos,Banned) :-
  find_min(Agenda,[M|T]),
  (setof(Child,children(M,Child,Task,T),Children) -> append(Children,T,NewAgenda) ;
  otherwise -> NewAgenda = T),
  solve_task_astar(Task,NewAgenda,RR,Cost,NewPos,Banned).

% the agent finishes with the task if the goal is reached, and the goal is not banned (for part 3)
achieved(go(Exit),Agenda,RPath,Cost,NewPos,_Banned) :-
  Agenda = [[Cost,_,NewPos,RPath]|_],
  ( Exit=none -> true
  ; otherwise -> RPath = [Exit|_]
  ).
achieved(find(O),Agenda,RPath,Cost,NewPos,Banned) :-
  Agenda = [[Cost,_,NewPos,RPath]|_],
  ( O=none    -> true
  ; otherwise -> RPath = [Last|_],map_adjacent(Last,_,O),not(member(O,Banned))
  ).

% looks for adjacent grid cells
search(F,N,N,1) :-
  map_adjacent(F,N,empty).
