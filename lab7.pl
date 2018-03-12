tree1(t(6, t(4, t(2, nil, nil), t(5, nil, nil)), t(9, t(7, nil, nil), nil))).
tree2(t(8, t(5, nil, t(7, nil, nil)), t(9, nil, t(11, nil, nil)))).

inorder(t(K,L,R), List):-inorder(L,LL), inorder(R, LR), 
							append(LL, [K|LR],List).
inorder(nil, []).

preorder(t(K,L,R), List):-preorder(L,LL), preorder(R, LR), 
							append([K|LL], LR, List).
preorder(nil, []).

postorder(t(K,L,R), List):-postorder(L,LL), postorder(R, LR), 
							append(LL, LR,R1), append(R1, [K], List).
postorder(nil, []).


% inorder traversal
pretty_print(nil, _).
pretty_print(t(K,L,R), D):-D1 is D+1, pretty_print(L, D1), print_key(K, D),
							pretty_print(R, D1).
% predicate which prints key K at D tabs from the screen left margin and then
% proceeds to a new line
print_key(K, D):-D>0, !, D1 is D-1, write('\t'), print_key(K, D1).
print_key(K, _):-write(K), nl.

search_key(Key, t(Key, _, _)):-!.
search_key(Key, t(K, L, _)):-Key<K, !, search_key(Key, L).
search_key(Key, t(_, _, R)):-search_key(Key, R).

insert_key(Key, nil, t(Key, nil, nil)):-write('Inserted '), write(Key), nl.
insert_key(Key, t(Key, L, R), t(Key, L, R)):-!, write('Key already in tree\n').
insert_key(Key, t(K, L, R), t(K, NL, R)):-Key<K, !, insert_key(Key, L, NL).
insert_key(Key, t(K, L, R), t(K, L, NR)):- insert_key(Key, R, NR).

delete_key(Key, nil, nil):-write(Key), write(' not in tree\n').
delete_key(Key, t(Key, L, nil), L):-!. % this clause covers also case for leaf (L=nil)
delete_key(Key, t(Key, nil, R), R):-!.
delete_key(Key, t(Key, L, R), t(Pred, NL, R)):-!, get_pred(L, Pred, NL).
delete_key(Key, t(K, L, R), t(K, NL, R)):-Key<K, !, delete_key(Key, L, NL).
delete_key(Key, t(K, L, R), t(K, L, NR)):- delete_key(Key, R, NR).
get_pred(t(Pred, L, nil), Pred, L):-!.
get_pred(t(Key, L, R), Pred, t(Key, L, NR)):-get_pred(R, Pred, NR).

max(A, B, A):-A>B, !.
max(_, B, B).

height(nil, 0).
height(t(_, L, R), H):-height(L, H1), height(R, H2), max(H1, H2, H3), H is H3+1.

%ternary trees
ttree1(t(6, t(4, t(2, nil, nil, nil), nil, t(7, nil, nil, nil)), t(5, nil, nil, nil), t(9, t(3, nil, nil, nil), nil, nil))).

pretty_print3(nil, _).
pretty_print3(t(K,L,M,R), D):-D1 is D+1, print_key3(K, D), 
				pretty_print3(L, D1), pretty_print3(M, D1), pretty_print3(R, D1).
				
print_key3(K, D):-D>0, !, D1 is D-1, write('\t'), print_key3(K, D1).
print_key3(K, _):-write(K), nl.

inorder3(nil, []).
inorder3(t(K, L, M, R), Res) :- inorder3(L, LL), inorder3(M, MM), inorder3(R, RR), 
								append(LL, [K|MM], Res1), append(Res1, RR, Res).
								
preorder3(nil, []).
preorder3(t(K, L, M, R), Res) :- preorder3(L, LL), preorder3(M, MM), preorder3(R, RR),
								append([K|LL], MM, Res1), append(Res1, RR, Res).
								
postorder3(nil, []).
postorder3(t(K, L, M, R), Res) :- postorder3(L, LL), postorder3(M, MM), postorder3(R, RR),
							append(LL, MM, Res1), append(Res1, RR, Res2), append(Res2, [K], Res).
							
height3(nil, 0).
height3(t(_, L, M, R), H):-height3(L, H1), height3(M, H2), height3(R, H3), 
				max(H1, H2, Max1), max(Max1, H3, Max), H is Max+1.
				
%exercises
inorder_print(t(K,L,R)):-inorder_print(L), write(K), write(' '), inorder_print(R).
inorder_print(nil).

delete_key2(Key, nil, nil):-write(Key), write(' not in tree\n').

delete_key2(Key, t(Key, L, nil), L):-!. % this clause covers also case for leaf (L=nil)
delete_key2(Key, t(Key, nil, R), R):-!.

delete_key2(Key, t(Key, L, R), Res):-!, insert_subtree(L, R, Res).
delete_key2(Key, t(K, L, R), t(K, NL, R)):-Key<K, !, delete_key2(Key, L, NL).
delete_key2(Key, t(K, L, R), t(K, L, NR)):- delete_key2(Key, R, NR).

insert_subtree(t(K, L, R), nil, t(K, L, R)).
insert_subtree(t(K1, L1, R1), t(K2, L2, R2), t(K2, LN, R2)) :-
				K1<K2, !, insert_subtree(t(K1, R1, L1), L2, LN).
insert_subtree(t(K1, L1, R1), t(K2, L2, R2), t(K2, L2, NR)):- 
				insert_subtree(t(K1, L1, R1), R2, NR).

collect_leaves(nil, []).				
collect_leaves(t(K, nil, nil), [K|_]).
collect_leaves(t(_, L, R), List) :- collect_leaves(L, LL), collect_leaves(R, LR), 
						append(LL, LR, List).
						
diameter(nil, 0).
diameter(t(_, L, R), D) :- 
			height(L, HL), height(R, HR),
			diameter(L, DL), diameter(R, DR),
			max(DL, DR, M1), H is HL + HR + 1,
			max(M1, H, D1),
			D is D1-1.