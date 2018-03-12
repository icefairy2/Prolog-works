perm_sort(L, R):-perm(L,R), is_ordered(R), !.

perm(L, [H|R]):-append(A, [H|T], L), append(A, T, L1), perm(L1, R).
perm([], []).

is_ordered([_]).
is_ordered([H1, H2|T]):-H1 =< H2, is_ordered([H2|T]).

minimum([], M, M).
minimum([H|T], MP, M):-H<MP, !, minimum(T, H, M).
minimum([_|T], MP, M):-minimum(T, MP, M).

minimum_pretty([H|T], R):-minimum([H|T], H, R).

delete4(X, [X|T], T):-!.
delete4(X, [H|T], [H|R]):-delete4(X, T, R).
delete4(_, [], []).

del_min(L, M, R) :- minimum_pretty(L, M), delete4(M, L, R).

sel_sort(L, [M|R]):- del_min(L, M, L1), sel_sort(L1, R), write([M|R]).
sel_sort([], []).

ins_sort([H|T], R):- ins_sort(T, R1), insert_ord(H, R1, R).
ins_sort([], []).

insert_ord(X, [H|T], [H|R]):-X>H, !, insert_ord(X, T, R).
insert_ord(X, T, [X|T]).

bubble_sort(L, R):-one_pass(L, R1, F), nonvar(F), !, bubble_sort(R1, R).
bubble_sort(L, L).

one_pass([H1, H2|T], [H2|R], F):- H1>H2, !, F = 1, one_pass([H1|T], R, F).
one_pass([H1|T], [H1|R], F):-one_pass(T, R, F).
one_pass([], [] ,_).

quick_sort([H|T], R):-partition(H, T, Sm, Lg), quick_sort(Sm, SmS),
	quick_sort(Lg, LgS), append(SmS, [H|LgS], R).
quick_sort([], []).

partition(H, [X|T], [X|Sm], Lg):-X<H, !, partition(H, T, Sm, Lg).
partition(H, [X|T], Sm, [X|Lg]):-partition(H, T, Sm, Lg).
partition(_, [], [], []).

merge_sort(L, R):-split(L, L1, L2), merge_sort(L1, R1), merge_sort(L2, R2),
	merge(R1, R2, R).
merge_sort([H], [H]).
merge_sort([], []).

split(L, L1, L2):-length(L, Len), Len>1, K is Len/2, splitK(L, K, L1, L2).

splitK([H|T], K, [H|L1], L2):- K>0, !, K1 is K-1, splitK(T, K1, L1, L2).
splitK(T, _, [], T).

merge([H1|T1], [H2|T2], [H1|R]):-H1<H2, !, merge(T1, [H2|T2], R).
merge([H1|T1], [H2|T2], [H2|R]):-merge([H1|T1], T2, R).
merge([], L, L).
merge(L, [], L).


%exercises

rnd_select_del(FROM, X, R):-
	random_member(X, FROM),
	delete4(X, FROM, R).
	
perm1(L, [X|R]):-rnd_select_del(L, X, L1), perm1(L1, R).
perm1([], []).

maximum([], M, M).
maximum([H|T], MP, M):-H>MP, !, maximum(T, H, M).
maximum([H|T], MP, M):-maximum(T, MP, M).

maximum_pretty([H|T], R):-maximum([H|T], H, R).

del_max(L, M, R) :- maximum_pretty(L, M), delete4(M, L, R).

sel_sortM(L, R):- del_max(L, M, L1), sel_sortM(L1, R1), append(R1, [M], R), write(M).
sel_sortM([], []).

ins_sort_fwd([H|T], S, R):-
	insert_ord(H, S, S1),
	ins_sort_fwd(T, S1, R).
	
ins_sort_fwd([], R, R).

ins_sortF(L, R):-
	ins_sort_fwd(L, [], R).

insert_ord(M, [H|T], [H|R]):-
	M > H, !,
	insert_ord(M, T, R).
	
insert_ord(M, T, [M|T]).

max([H|T], M):-
	char_code(H, H1),
	max(T, M),
	char_code(M, M1),
	H1 =< M1, !.
	
max([H|_], H).

char_sort(L, S, R):-
	max(L, M),
	delete4(M, L, R1),
	char_sort(R1, [M|S], R).
	
char_sort([], R, R).

char_sort(L,  R):-
	char_sort(L, [], R).
	
	
sort_len(L, S, R):-
	maxl(L, M),
	delete4(M, L, R1),
	sort_len(R1, [M|S], R).
	
sort_len([], R, R).

maxl([H|T], M):-
	length(H, H1),
	maxl(T, M),
	length(M, M1),
	H1 =< M1, !.
	
sort_len(L,  R):-
	sort_len(L, [], R).