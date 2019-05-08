
candidate_number(12345).

get_energy(E) :- my_agent(Agent),query_world( agent_current_energy, [Agent,E] ).

recharge :-   my_agent(Agent),
              query_world( agent_current_position, [Agent,P] ),
              solve_task_astar(find(c(I)),[[0,_,P,[P]]],R,_Cost,_NewPos,[]),!,
              reverse(R,[_Init|Path]),
              query_world( agent_do_moves, [Agent,Path] ),
              query_world( agent_topup_energy, [Agent,c(I)] ).

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

children([G,_,P,RPath],[G1,H1,P1,[P1|RPath]],Task,Agenda) :- search(P,P1,_,_), G1 is G + 1,
                                                      heuristics(P1,H1,Task),
                                                      not(member([_,_,P1,_],Agenda)).

heuristics(P,H1,go(Exit)) :- map_distance(P,Exit,H1).
heuristics(_,0,find(_)).

find_min([H|T],Result) :- find_min_aux(H,[],T,Result).
find_min_aux(Smallest,Seen,[],[Smallest|Seen]).
find_min_aux(Smallest,Seen,[H|T],Result) :- lt(H,Smallest),!,find_min_aux(H,[Smallest|Seen],T,Result) ; find_min_aux(Smallest,[H|Seen],T,Result).
lt([G1,H1,_,_],[G2,H2,_,_]) :- F1 is G1 + H1, F2 is G2 + H2, F1 < F2.

solve_task_astar(Task,Agenda,RPath,[cost(Cost),depth(Cost)],NewPos,Banned) :-
  find_min(Agenda,TopFirst),
  achieved(Task,TopFirst,RPath,Cost,NewPos,Banned).
solve_task_astar(Task,Agenda,RR,Cost,NewPos,Banned) :-
  find_min(Agenda,[M|T]),
  (setof(Child,children(M,Child,Task,T),Children) -> append(Children,T,NewAgenda) ;
  otherwise -> NewAgenda = T),
  solve_task_astar(Task,NewAgenda,RR,Cost,NewPos,Banned).

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

search(F,N,N,1) :-
map_adjacent(F,N,empty).
