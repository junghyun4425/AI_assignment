candidate_number(12345).

solve_task(Task,Cost):-
  my_agent(Agent),
  query_world( agent_current_position, [Agent,P] ),
  solve_task_bt(Task,[[0,0,P,[P]]],R,Cost,_NewPos),!,  % prune choice point for efficiency
  reverse(R,[_Init|Path]),
  query_world( agent_do_moves, [Agent,Path] ).


children([_,ParG,ParP,ParRPath],[ChildF,ChildG,ChildP,[ChildP|ParRPath]]) :- ChildG is ParG + 1,
                                              search(ParP,ChildP,_,_),
                                              heuristics(ChildP,H), ChildF is H + ChildG.


heuristics(p(X,Y),H) :- H is 0. % BFS


findMin([H|T],Result) :- findMinAux(H,[],T,Result).
findMinAux(Smallest,Seen,[],[Smallest|Seen]).
findMinAux(Smallest,Seen,[H|T],Result) :- leq(H,Smallest),!,findMinAux(H,[Smallest|Seen],T,Result) ; findMinAux(Smallest,[H|Seen],T,Result).
leq([F1,_,_,_],[F2,_,_,_]) :- F1 < F2.

% A STAR
solve_task_bt(Task,Agenda,RPath,[cost(Cost),depth(Cost)],NewPos) :-
  findMin(Agenda,TopFirst),
  achieved(Task,TopFirst,RPath,Cost,NewPos).
solve_task_bt(Task,Agenda,RR,Cost,NewPos) :-
  findMin(Agenda,[M|T]),
  setof(Child,children(M,Child),Children),
  append(Children,T,NewAgenda),
  solve_task_bt(Task,NewAgenda,RR,Cost,NewPos).

achieved(go(Exit),Agenda,RPath,Cost,NewPos) :-
  Agenda = [[_,Cost,NewPos,RPath]|_],
  ( Exit=none -> true
  ; otherwise -> RPath = [Exit|_]
  ).
achieved(find(O),Agenda,RPath,Cost,NewPos) :-
  Agenda = [[_,Cost,NewPos,RPath]|_],
  ( O=none    -> true
  ; otherwise -> RPath = [Last|_],map_adjacent(Last,_,O)
  ).

search(F,N,N,1) :-
  map_adjacent(F,N,empty).
