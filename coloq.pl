max_occur_elem(L, X):-
		flatten(L, FL),
		group_occurences(FL, GL),
		get_max_occur(GL, M).
		get_elem_max_occur(GL, M, X).
		
flatten(L, []) :- var(L), !.
flatten([H|T], [H|R]) :- atomic(H), !, 
		flatten(T, R).
flatten([H|T], R) :-
		flatten(H, HR), flatten(T, TR),
		append(HR, TR, R).
		
group_occurences([], []).
group_occurences([H|T], R) :-
		get_occurences(T, H, 1, OC),
		delete_all([H|T], H, DL),
		group_occurences(DL, RES),
		append([[H, OC]], RES, R).
		
get_occurences([], _, ACC, ACC).
get_occurences([H|T], H, ACC, R) :-
		ACC1 is ACC + 1, !, 
		get_occurences(T, H, ACC1, R).
get_occurences([H|T], X, ACC, R) :-
		get_occurences(T, X, ACC, R).
		
delete_all([], _, []).
delete_all([H|T], H, R) :- !,
		delete_all(T, H, R).
delete_all([H|T], X, [H|R]):-
		delete_all(T, X, R).
		
get_max_occur([], M, M).
get_max_occur([[H, HOC]|T], M, RM) :-
		HOC>M, !,
		get_max_occur(T, HOC, RM).
get_max_occur([[H, HOC]|T], M, RM) :-
		get_max_occur(T, M, RM).
		
get_elem_max_occur([[H, M]|T], M, H).
get_elem_max_occur([[H, HOC]|T], M, RE) :-
		get_elem_max_occur(T, M, RE).
		
sum_bst(t(K, nil, nil), S):-
		RS is S+K, assert(sum(RS)).
sum_bst(t(K, L, R), S) :-
		IS is S+K,
		sum_bst(L, IS).
		sum_bst(R, IS).
		
group_sum_bst(L, RL) :- retract(sum(X)),!,  append([X], L, IL),
		group_sum_bst(IL, RL).
group_sum_bst(RL, RL).
		
get_max_list([], M, M).
get_max_list([H|T], M, RM) :-
		H>M, !, get_max_list(T, H, RM).
get_max_list(get_max_list([H|T], M, RM) :-
		get_max_list(T, M, RM).
		
get_max_sum_bst(T, M) :-
		sum_bst(T, 0),
		group_sum_bst([],[H|T]),
		get_max_list(T, H, M).