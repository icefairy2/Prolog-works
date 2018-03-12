member14(X, [X|_]):-!.
member14(X, [_|T]):-member14(X, T).

delete4(X, [X|T], T):-!.
delete4(X, [H|T], [H|R]):-delete4(X, T, R).
delete4(_, [], []).

length4([],0).
length4([H|T], Len):-length4(T,Len1), Len is Len1+1.

% when reaching the empty list, unify accumulator with the free result variable
length_fwd([], Acc, Res):-Res = Acc.
% as the list is decomposed, add 1 to the accumulator; pass Res unchanged
length_fwd([H|T], Acc, Res):-Acc1 is Acc+1, length_fwd(T, Acc1, Res).

length_fwd_pretty(L, Len):-length_fwd(L, 0, Len).

reverse4([], []).
reverse4([H|T], Res):-reverse4(T, R1), append(R1, [H], Res).

reverse_fwd([], R, R).
reverse_fwd([H|T], Acc, R):-reverse_fwd(T, [H|Acc], R).

reverse_fwd_pretty(L, R):- reverse_fwd(L, [], R).

minimum([], M, M).
minimum([H|T], MP, M):-H<MP, !, minimum(T, H, M).
minimum([H|T], MP, M):-minimum(T, MP, M).

minimum_pretty([H|T], R):-minimum([H|T], H, R).

minimum_bwd([H], H).
minimum_bwd([H|T], M):-minimum_bwd(T, M), H>=M.
minimum_bwd([H|T], H):-minimum_bwd(T, M), H<M.

union([ ],L,L).
union([H|T],L2,R) :- member(H,L2),!,union(T,L2,R).
union([H|T],L,[H|R]):-union(T,L,R).

inters([],L,[]):-!.
inters([H|T],L2,[H|R]) :- member(H,L2),!,inters(T,L2,R).
inters([H|T],L,R):-inters(T,L,R).

set_diff([],L,[]).
set_diff([H|T],L2,R) :- member(H,L2),!,set_diff(T,L2,R).
set_diff([H|T],L,[H|R]):-set_diff(T,L,R).

%exercises

del_min(L, M, R) :- minimum_pretty(L, M), delete4(M, L, R).

rev_k(L, 1, R):- reverse4(L, R).
rev_k([H|T], K, [H|R]):- K1 is K - 1, !, rev_k(T, K1, R).

maximum([], M, M).
maximum([H|T], MP, M):-H>MP, !, maximum(T, H, M).
maximum([H|T], MP, M):-maximum(T, MP, M).

maximum_pretty([H|T], R):-maximum([H|T], H, R).

del_max(L, M, R) :- maximum_pretty(L, M), delete4(M, L, R).

rle_encode_helper([H|T], H, ACC, R):- !,
	ACC1 is ACC +1,
	rle_encode_helper(T, H, ACC1, R).
	
rle_encode_helper([H|T], X, ACC, [[X, ACC]|R]):-
		rle_encode_helper(T, H, 1, R).
rle_encode([], H, ACC, [[H, ACC]]).

rle_encode([H|T], R):-
		rle_encode_helper(T, H, 1, R).
	
rotate_right_k(L, 0, L).
rotate_right_k([H|T], K, R):- 
	K>0, append(T, [H], R1),
	K1 is K-1,
	rotate_right_k(R1, K1, R).
	
rotate_right(L, K, R):-
	length4(L, N),
	K1 is N-K,
	rotate_right_k(L, K1, R).
	
rnd_select(_, 0, []).

rnd_select(FROM, C, [X|R]):-
	random_member(X, FROM),
	select(X, FROM, RN),
	C1 is C - 1,
	rnd_select(RN, C1, R).
