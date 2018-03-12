collect_lists(L,[]):- var(L), !.
collect_lists([H|T], R) :- atomic(H),!, collect_lists(T,R).
collect_lists([H|T], [H|R]) :- collect_lists(T,R).


sum_list([],0).
sum_list([H|_],R):- \+atomic(H),!,sum_list(H,R).
sum_list([H|T],R):-sum_list(T,R1),R is H + R1.

get_list_sum_min(L,[]):-var(L), !.
get_list_sum_min([H|T],R):-atomic(H),!, get_list_sum_min(T,R).
get_list_sum_min([H|T],R):-sum_list(H,HS),get_list_sum_min(T,TS),append([HS],TS,R).


min_list(L,R):-get_list_sum_min(L,[H|T]),min_list1(T,H,R).

min_list1([],X,X).
min_list1([H|T],Acc,M):-H<Acc,!,min_list1(T,H,M).
min_list1([_|T],Acc,M):-min_list1(T,Acc,M).


tree1(t(6, t(4, t(2, nil, nil), t(5, nil, nil)), t(9, t(7, nil, nil), nil))).

max(A,B,A):-A>B,!.
max(_,B,B).

height(nil,0).
height(t(K,L,R),H):-height(L,H1),height(R,H2),max(H1,H2,H3),H is H3 + 1.

collect_h(nil,_,[]).
collect_h(t(K,L,R),H,[K]):-height(t(K,L,R),CH),H=CH,!.
collect_h(t(K,L,R),H,RR):-collect_h(L,H,LSL),collect_h(R,H,LSR),append(LSL,LSR,RR).


bst_to_dl(nil,[]).
bst_to_dl(t(K,L,R),DL):- bst_to_dl(L,DLL), bst_to_dl(R,DLR), DL=[K,DLL,DLR].


count_lists([],1).
count_lists([H|T], R) :- atomic(H), !, count_lists(T, R).
count_lists([H|T], R) :- count_lists(H, R1), count_lists(T, R2), R is R1 + R2.

numbers([],[]).
numbers([H|T], R) :- H mod 2 =:= 0, !,
		numbers(T, R1), N is H*H, R =[N|R1].
numbers([H|T], R) :- 
		numbers(T, R1), N is 2*H, R =[N|R1].

to_binary(0, [0]) :- !.
to_binary(1, [1]) :- !.
to_binary(N, R) :- N1 is N div 2, to_binary(N1, R1),
		M is N mod 2, append(R1, [M], R).
		
replace_all(_, LS, LS, _, []).
replace_all(X, [X|T], E, Y, R) :- 
		replace_all(X, T, E, Y, R1), !,
		append([Y, X, Y], R1, R).
replace_all(X, [H|T], E, Y, [H|R]) :- 
		replace_all(X, T, E, Y, R).

delete_pos_even([], _, []).
delete_pos_even([X|T], X, R):-
		length([X|T], N), N mod 2 =:= 0, !, delete_pos_even(T, X, R).
delete_pos_even([H|T], X, [H|R]) :- delete_pos_even(T, X, R).

divisor(N, R) :- N<0, !, N1 is N*(-1), divisor(N1, R).
divisor(0, 'alot').
divisor(N, R) :- divisor1(N,N,R).

divisor1(_, 1, [1]) :-!.
divisor1(N, K, R) :- N mod K =:= 0, !, K1 is K-1, divisor1(N, K1, R1), append(R1, [K], R).
divisor1(N, K, R) :- K1 is K-1, divisor1(N, K1, R).

decompose(0, []) :- !.
decompose(N, L):-N1 is N div 10, decompose(N1, R), D is N mod 10, append(R, [D], L).

construct([], 0).
construct([H|T], R):-construct(T, R1), R is R1*10+H.

reverseN(N, R) :- decompose(N, L), construct(L, R).

delete_kth_end([], _, []).
delete_kth_end([X|T], K, R):-
		length([X|T], N), N mod K =:= 0, !, delete_kth_end(T, K, R).
delete_kth_end([H|T], X, [H|R]) :- delete_kth_end(T, X, R).

separate([],[],[]).
separate([H|T], [H|E], R) :- length([H|T], N), H mod 2 =:= 0, N mod 2 =:= 1, !,
		separate(T, E, R).
separate([H|T], E, [H|R]) :- separate(T, E, R).

%tree(t(26,t(14,t(2,_,_),t(15,_,_)),t(50,t(35,t(29,_,_),_),t(51,_,t(58,_,_))))).

collect(T,_) :- var(T) ,!.
collect(t(K, L, R), [K|_]) :- var(L), nonvar(R), K mod 2 =:= 1, !.
collect(t(K, L, R), [K|_]) :- var(R), nonvar(L), K mod 2 =:= 1, !.
collect(t(K, L, R), Res) :- collect(L, LR), collect(R, RR), append_il(LR, RR, Res).

append_il(L, L2, L2) :- var(L), !.
append_il([H|T], L2, [H|R]) :- append_il(T, L2, R).

%tree(t(2,t(8,_,_,_),t(3,_,_,t(4,_,_,_)),t(5,t(7,_,_,_),t(6,_,_,_),t(1,_,_,t(9,_,_,_))))).

collect_between(T, _, _, L, L) :- var(T), !.
collect_between(t(K, L, M, R), X, Y, [K|LS], LE) :-
		K >= X, K =< Y, !,
		collect_between(L, X, Y, LSL, LEL),
		collect_between(M, X, Y, LSM, LEM),
		collect_between(R, X, Y, LSR, LER),
		LS = LSL, LEL = LSM, LEM = LSR, LE = LER.
collect_between(t(_, L, M, R), X, Y, LS, LE) :-
		collect_between(L, X, Y, LSL, LEL),
		collect_between(M, X, Y, LSM, LEM),
		collect_between(R, X, Y, LSR, LER),
		LS = LSL, LEL = LSM, LEM = LSR, LE = LER.
	
%tree(t(5,t(10,t(7,nil,nil),t(10,t(4,nil,nil),t(3,nil,t(2,nil,nil)))),t(16,nil,nil))).

collect_even(nil, L, L).
collect_even(t(K, nil, nil), [K|LS], LS) :- K mod 2 =:= 0, !.
collect_even(t(K, L, R), LS, LE) :- collect_even(L, LS, LT), collect_even(R, LT, LE).

%tree(t(2,t(8,_,_,_),t(3,_,_,t(1,_,_,_)),t(5,t(7,_,_,_),t(6,_,_,_),t(1,_,_,t(9,_,_,_))))).

get_min(T, M, M) :- var(T), !.
get_min(t(K, L, M, R), Min, Res) :-		
		min(K, Min, Min1), 
		get_min(L, Min1, M1), 
		get_min(M, M1, M2), 
		get_min(R, M2, Res).
		
		
min(A, B, A) :- A<B, !.
min(A, B, B).

replace_key(T, _, _, _) :- var(T), !.
replace_key(t(K, L, M, R), K, Root, t(Root, L, M, R)) :- !.
replace_key(t(K, L, M, R), Min, Root, Res) :-
		replace_key(L, Min, Root, Res1),
		replace_key(M, Min, Root, Res2),
		replace_key(R, Min, Root, Res3),
		Res = t(K, Res1, Res2, Res3).

		
replace_min(t(Root, L, M, R), Res) :-
		get_min(t(Root, L, M, R), Root, Min),
		replace_key(t(Root, L, M, R), Min, Root, Res).

%tree(t(26,t(14,t(2,_,_),t(15,_,_)),t(50,t(35,t(29,_,_),_),t(51,_,t(58,_,_))))).
		
depth_key(t(K, _, _), K, R, R) :- !.
depth_key(t(K, L, R), Key, PRes, Res) :- Key<K, !, PR1 is PRes + 1, depth_key(L, Key, PR1, Res).
depth_key(t(K, L, R), Key, PRes, Res) :- PR1 is PRes + 1, depth_key(R, Key, PR1, Res).

collect_all(T, L) :- collect_all(T,T,L).
collect_all(T, _, []) :- var(T), !.
collect_all(t(K, L, R), t(Orig, LO, RO), [K|Res]) :-
		depth_key(t(Orig, LO, RO), K, 0, H),
		H mod 2 =:= 1, !,
		collect_all(L, t(Orig, LO, RO), LL), collect_all(R, t(Orig, LO, RO), RL), 
		append(LL, RL, Res).
collect_all(t(K, L, R), t(Orig, LO, RO), Res) :-
		collect_all(L, t(Orig, LO, RO), LL), collect_all(R, t(Orig, LO, RO), RL), 
		append(LL, RL, Res).

flatten_depth([], _, []).
flatten_depth([H|T], 1, [H|R]) :- atomic(H), !, flatten_depth(T, 1, R).
flatten_depth([H|T], X, R) :- atomic(H), !, flatten_depth(T, X, R).
flatten_depth([H|T], X, R) :- X1 is X-1, flatten_depth(H, X1, R1), flatten_depth(T, X, R2),
		append(R1, R2, R).
		
delete_dup_key([], []).
delete_dup_key([H], []) :- n(H), !.
delete_dup_key([H], [H]).
delete_dup_key([H, X|T], [X|R]) :-
		member(H, [X|T]), !,
		assertz(n(H)),
		delete_dup_key(T, R).
delete_dup_key([H, X|T], [X|R]):-
		n(H), !,
		delete_dup_key(T, R).
delete_dup_key([H, X|T], [H, X|R]):-
		delete_dup_key(T, R).

%tree(t(2,t(8,_,_,_),t(3,_,_,t(1,_,_,_)),t(5,t(7,_,_,_),t(5,_,_,_),t(1,_,_,t(9,_,_,_))))).
		
get_min(t(K, L, M, R), Min) :- get_min(t(K, L, M, R), K, Min).
		
get_max(T, M, M) :- var(T), !.
get_max(t(K, L, M, R), Max, Res) :-		
		max(K, Max, Max1), 
		get_max(L, Max1, M1), 
		get_max(M, M1, M2), 
		get_max(R, M2, Res).
		
get_max(t(K, L, M, R), Max) :- get_max(t(K, L, M, R), K, Max).

median(T, L) :- get_min(T, Min), get_max(T, Max), Med is (Min+Max) div 2, median(T, Med, L).

median(T, _, []) :- var(T), !.
median(t(Med, L, M, R), Med, Res) :- !,
		median(L, Med, LLS), median(M, Med, MLS), median(R, Med, RLS),
		append(MLS, RLS, MR), append(LLS, MR, LMR), append([t(Med, L, M, R)], LMR, Res).
median(t(X, L, M, R), Med, Res) :- 
		median(L, Med, LLS), median(M, Med, MLS), median(R, Med, RLS),
		append(MLS, RLS, MR), append(LLS, MR, Res).		
		
height_it(T, -1) :- var(T), !.
height_it(t(K, L, R), H) :-
		height_it(L, HL), 
		height_it(R, HR),
		max(HL, HR, M),
		H is M+1.
	
tree(t(2,t(4,t(5,_,_),t(7,_,_)),t(3,t(0,t(4,_,_),_),t(8,_,t(5,_,_))))).
	
height_each(T, T) :- var(T), !.
height_each(t(K, L, R), Res):- height_it(t(K, L, R), H),
		height_each(L, LT), height_each(R, RT),
		Res = t(H, LT, RT).
		
len_con_depth([], A, [A]).
len_con_depth([H|T], ACC, R) :-
		atomic(H), !,
		ACC1 is ACC+1,
		len_con_depth(T, ACC1, R).
len_con_depth([H|T], ACC, [ACC|R]) :- ACC>0, !,
		len_con_depth(H, 0, R1),
		len_con_depth(T, 0, R2),
		append([R1], R2, R).
len_con_depth([H|T], 0, R) :-
		len_con_depth(H, 0, R1),
		len_con_depth(T, 0, R2),
		append([R1], R2, R).
		
len_con_depth(L, R) :- len_con_depth(L, 0, R).

construct_list([_, 0], []) :- !.
construct_list([E, L], [E|R]) :- L1 is L-1, construct_list([E, L1], R).

rle_decode([], []).
rle_decode([H|T], R) :- construct_list(H, L), rle_decode(T, RL), append(L, RL, R).

rle_encode_helper([], H, ACC, [[H, ACC]]).
rle_encode_helper([H|T], H, ACC, R):- !,
	ACC1 is ACC +1,
	rle_encode_helper(T, H, ACC1, R).
rle_encode_helper([H|T], X, ACC, [[X, ACC]|R]):-
		rle_encode_helper(T, H, 1, R).

rle_encode([H|T], R):-
		rle_encode_helper(T, H, 1, R).
	
edge(1,2). edge(2,1). edge(1,4). edge(1,3). edge(3,2).

:-dynamic info/3.
	
store_info :- edge(M, N), update_info_out(M, N), update_info_in(M, N), fail.
store_info.

update_info_out(M, N) :- info(M, MO, MI), !, retract(info(M, MO, MI)), MO1 is MO + 1,
		assert(info(M, MO1, MI)).
update_info_out(M, N) :- assert(info(M, 1, 0)).

update_info_in(M, N) :- info(N, NO, NI), !, retract(info(N, NO, NI)), NI1 is NI + 1,
		assert(info(N, NO, NI1)).
update_info_in(M, N) :- assert(info(N, 0, 1)).