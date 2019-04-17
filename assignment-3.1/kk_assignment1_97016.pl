candidate_number(97016).
q1(ailp_start_position(_P)).
q2a(new_pos(p(1,1),e,_X)).
q2b(136).
q3([e,n,w,s]).
q4a([p(1,4),p(2,4),p(3,4),p(4,4),p(4,3),p(3,3),p(2,3),p(1,3),p(1,2),p(2,2),p(3,2),p(4,2),p(4,1),p(3,1),p(2,1),p(1,1)]).
q4b([p(1,4),p(2,4),p(3,4),p(4,4),p(4,3),p(3,3),p(2,3),p(1,3),p(1,2),p(2,2),p(3,2),p(3,1),p(4,1),p(4,2)]).
q4c([p(1,4),p(2,4),p(3,4),p(4,4),p(4,3),p(3,3),p(2,3),p(1,3),p(1,2),p(2,2),p(3,2),p(4,2),p(4,1),p(3,1),p(2,1),p(1,1)]).
q4d([p(1,4),p(2,4),p(3,4),p(4,4),p(4,3),p(3,3),p(2,3),p(1,3),p(1,2),p(1,1),p(2,1),p(2,2),p(3,2),p(4,2),p(4,1),p(3,1)]).
q5_corner_move :- ailp_grid_size(N),ailp_show_move(p(1,1),p(1,N)),ailp_show_move(p(1,N),p(N,1)),ailp_show_move(p(N,1),p(N,N)).
q6_spiral(L) :-
  C is 1, ailp_start_position(p(X,Y)), q6_spiral(p(X,Y),L,C).
q6_spiral(P,L,C) :-
  q6_spiral(P,[P],Ps,C),
  reverse(Ps,L).
q6_spiral(P,Ps,R,C) :-
  K is C+1,
  ((C < 4,new_pos(P,n,P1));
  (C >= 4,C < 7,new_pos(P,e,P1));
  (C >= 7, C < 9,new_pos(P,s,P1));
  (C >= 9, C < 11,new_pos(P,w,P1));
  (C == 11, new_pos(P,n,P1));
  (C == 12, new_pos(P,e,P1))),
  ailp_show_move(P,P1),
  q6_spiral(P1,[P1|Ps],R,K);C == 13.
