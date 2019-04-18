candidate_number(12345).

solve_task(Task,Cost):-
  my_agent(Agent),
  query_world( agent_current_position, [Agent,P] ),
  solve_task_astar(Task,[[0,_,P,[P]]],R,Cost,_NewPos),!,  % prune choice point for efficiency
  reverse(R,[_Init|Path]),
  query_world( agent_do_moves, [Agent,Path] ).


% Find whether a position P is in the agenda
in_list(P,[[_,_,P,_]|_]).
in_list(P,[_|T]) :- in_list(P,T).

children([G,_,P,RPath],[G1,H1,P1,[P1|RPath]],Task,Agenda) :- search(P,P1,_,_), G1 is G + 1,
                                                      heuristics(P1,H1,Task),
                                                      not(in_list(P1,Agenda)).

target(T,go(Exit)) :- T = Exit.
target(T,find(O)) :- query_world( check_pos, [T, O]). % It does not work, cannot find the T

heuristics(P,H1,go(Exit)) :- map_distance(P,Exit,H1).
heuristics(_,0,find(_)).

find_min([H|T],Result) :- find_min_aux(H,[],T,Result).
find_min_aux(Smallest,Seen,[],[Smallest|Seen]).
find_min_aux(Smallest,Seen,[H|T],Result) :- lt(H,Smallest),!,find_min_aux(H,[Smallest|Seen],T,Result) ; find_min_aux(Smallest,[H|Seen],T,Result).
lt([G1,H1,p(X1,Y1),_],[G2,H2,p(X2,Y2),_]) :- F1 is G1 + H1, F2 is G2 + H2, (F1 < F2 ; F1 = F2, X1 < X2 ; F1 = F2, X1 = X2, Y1 < Y2).

solve_task_astar(Task,Agenda,RPath,[cost(Cost),depth(Cost)],NewPos) :-
  find_min(Agenda,TopFirst),
  achieved(Task,TopFirst,RPath,Cost,NewPos).
solve_task_astar(Task,Agenda,RR,Cost,NewPos) :-
  find_min(Agenda,[M|T]),
  (setof(Child,children(M,Child,Task,T),Children) -> append(Children,T,NewAgenda) ;
  otherwise -> NewAgenda = T),
  solve_task_astar(Task,NewAgenda,RR,Cost,NewPos).

achieved(go(Exit),Agenda,RPath,Cost,NewPos) :-
  Agenda = [[Cost,_,NewPos,RPath]|_],
  ( Exit=none -> true
  ; otherwise -> RPath = [Exit|_]
  ).
achieved(find(O),Agenda,RPath,Cost,NewPos) :-
  Agenda = [[Cost,_,NewPos,RPath]|_],
  ( O=none    -> true
  ; otherwise -> RPath = [Last|_],map_adjacent(Last,_,O)
  ).

search(F,N,N,1) :-
  map_adjacent(F,N,empty).
