%L1 = [1,2,3,[4]]
%L2 = [[1],[2],[3],[4,5]]
%L3 = [[],2,3,4,[5,[6]],[7]]
%L4 = [[[[1]]],1, [1]]
%L5 = [1,[2],[[3]],[[[4]]],[5,[6,[7,[8,[9],10],11],12],13]]
%L6= [alpha, 2,[beta],[gamma,[8]]]

delete1(X, [X|T], T).
%traverse the list, add the elements H≠X back to the result
delete1(X, [H|T], [H|R]):-delete1(X, T, R).
delete1(_, [], []).

max(A, B, R) :- A > B, R is A.
max(A, B, R) :- A =< B, R is B.

depth([],1).
depth([H|T],R):-atomic(H),!,depth(T,R).
depth([H|T],R):- depth(H,R1), depth(T,R2), R3 is R1+1, max(R3,R2,R).

flatten([],[]).
flatten([H|T], [H|R]) :- atomic(H),!, flatten(T,R).
flatten([H|T], R) :- flatten(H,R1), flatten(T,R2), append(R1,R2,R).

heads3([],[],_).
heads3([H|T],[H|R],1):-atomic(H),!,heads3(T,R,0).
heads3([H|T],R,0):-atomic(H),!,heads3(T,R,0).
heads3([H|T],R,_):-heads3(H,R1,1),heads3(T,R2,0), append(R1,R2,R).
heads_pretty(L,R) :- heads(L, R,1).

member1(H,[H|_]).
member1(X,[H|_]):-member1(X,H).
member1(X,[_|T]):-member1(X,T).

member2(X,L):- flatten(L,L1), member(X,L1).

%exercises
nr_atom([], R, R).
nr_atom([H|T],PR, R) :- atomic(H), !, PR1 is PR+1, nr_atom(T, PR1, R).
nr_atom([H|T], PR, R) :- nr_atom(H, PR, R1), nr_atom(T, R1, R).
nr_atom_pretty(L, R) :- nr_atom(L, 0, R).

sum_atom([], R, R).
sum_atom([H|T],PR, R) :- atomic(H), !, PR1 is PR+H, sum_atom(T, PR1, R).
sum_atom([H|T], PR, R) :- sum_atom(H, PR, R1), sum_atom(T, R1, R).
sum_atom_pretty(L, R) :- sum_atom(L, 0, R).

member1_det(H,[H|_]):-!.
member1_det(X,[H|_]):-member1_det(X,H).
member1_det(X,[_|T]):-member1_det(X,T).

end_shallow([], []).
end_shallow([H|[]], [H|_]) :- atomic(H).
end_shallow([H|T], R):- atomic(H), !, end_shallow(T, R).
end_shallow([H|T], R):- end_shallow(H, R1), end_shallow(T, R2), append(R1, R2, R), !.

replace(_, _, [], []).
replace(S, R, [S | T], [R | RL]):-!, replace(S, R, T, RL).
replace(S, R, [H | T], [H | RL]):- atomic(H), !, replace(S, R, T, RL).
replace(S, R, [H | T], [RH | RT]):-	replace(S, R, H, RH), replace(S, R, T, RT).

%optional exercise
%order by depth