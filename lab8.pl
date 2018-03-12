% must test explicitly for the end of the list, and fail
member_il(_, L):-var(L), !, fail.
% these 2 clauses are the same as for the member1 predicate
member_il(X, [X|_]):-!.
member_il(X, [_|T]):-member_il(X, T).

insert_il(X, L):-var(L), !, L=[X|_]. % found end of list, add element
insert_il(X, [X|_]):-!. % found element, stop
insert_il(X, [_|T]):- insert_il(X, T). % traverse input list to reach end or X

delete_il(_, L, L):-var(L), !. % reached end, stop
delete_il(X, [X|T], T):-!. % found element, remove it and stop
delete_il(X, [H|T], [H|R]):-delete_il(X, T, R). % search for the element

search_it(_, T):-var(T), !, fail.
search_it(Key, t(Key, _, _)):-!.
search_it(Key, t(K, L, _)):-Key<K, !, search_it(Key, L).
search_it(Key, t(_, _, R)):-search_it(Key, R).

insert_it(Key, t(Key, _, _)):-!.
insert_it(Key, t(K, L, _)):-Key<K, !, insert_it(Key, L).
insert_it(Key, t(_, _, R)):- insert_it(Key, R).

delete_it(Key, T, T):-var(T), !, write(Key), write(' not in tree\n').
delete_it(Key, t(Key, L, R), L):-var(R), !.
delete_it(Key, t(Key, L, R), R):-var(L), !.
delete_it(Key, t(Key, L, R), t(Pred, NL, R)):-!, get_pred(L, Pred, NL).
delete_it(Key, t(K, L, R), t(K, NL, R)):-Key<K, !, delete_it(Key, L, NL).
delete_it(Key, t(K, L, R), t(K, L, NR)):- delete_it(Key, R, NR).
get_pred(t(Pred, L, R), Pred, L):-var(R), !.
get_pred(t(Key, L, R), Pred, t(Key, L, NR)):-get_pred(R, Pred, NR).

%exercises
append_il(L1, L2, L2) :- var(L1), !.
append_il([H|T], L2, [H|R]) :- append_il(T, L2, R).

reverse_il(L, []) :- var(L), !.
reverse_il([H|T], R) :- reverse_il(T, R1), append(R1, [H|_], R).

transform_il(L, []) :- var(L), !.
transform_il([H|T], [H|R]) :- transform_il(T, R).

preorder_it(T, T):- var(T), !.
preorder_it(t(K,L,R), List):-preorder_it(L,LL), preorder_it(R, LR), 
							append([K|LL], LR, List).
							
height_il(T, 0):-var(T),!.
height_il(t(_, L, R), Rez):-height_il(L, R1),
			       height_il(R, R2),
			       max2(R1, R2, R3),
			       Rez is R3 + 1.
				   
transform_it(T, nil) :- var(T), !.
transform_it(t(K, L, R), t(K, LL, RR)) :- transform_it(L, LL), transform_it(R, RR).

flat_il(L, _) :- var(L), !.
flat_il([H|T], [H|R]) :- atomic(H), !, flat_il(T, R).
flat_il([H|T], R) :- flat_il(H,R1), flat_il(T,R2), append_il(R1,R2,R).

max(A, B, A):-A>B, !.
max(_, B, B).

diameter_it(T, -1):- var(T), !.
diameter_it(t(_, L, R), D) :- 
			height_il(L, HL), height_il(R, HR),
			diameter_it(L, DL), diameter_it(R, DR),
			max(DL, DR, M1), H is HL + HR + 1,
			max(M1, H, D).

%optional exercises