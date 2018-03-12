add(X,LS,LE,RS,RE):- RS=LS, LE=[X|RE].

/* when we reached the end of the tree we unify the beggining and end of the partial result list – representing an empty list as a difference list */
inorder_dl0(nil,L,L).
inorder_dl0(t(K,L,R),LS,LE):-
/* obtain the start and end of the lists for the left and right subtrees */
inorder_dl0(L,LSL,LEL),
inorder_dl0(R,LSR,LER),
/* the start of the result list is the start of the left subtree list */
LS=LSL,
/* insert the key between the end of the left subtree list and start of the right subtree list */
LEL=[K|LSR],
/* the end of the result list is the end of the right subtree list */
LE=LER.

inorder_dl(nil,L,L).
inorder_dl(t(K,L,R),LS,LE):-inorder_dl(L,LS,[K|LT]), inorder_dl(R,LT,LE).

preorder_dl(nil,L,L).
preorder_dl(t(K,L,R),[K|LS],LE):-preorder_dl(L,LS,LT), preorder_dl(R,LT,LE).

postorder_dl(nil,L,L).
postorder_dl(t(K,L,R),LS,LE):-postorder_dl(L,LS,LT), postorder_dl(R,LT,[K|LE]).

quicksort_dl([H|T],S,E):-
	partition(H,T,Sm,Lg),
	quicksort_dl(Sm,S,[H|L]),
	quicksort_dl(Lg,L,E).
quicksort_dl([],L,L).

partition(H, [X|T], [X|Sm], Lg):-X<H, !, partition(H, T, Sm, Lg).
partition(H, [X|T], Sm, [X|Lg]):-partition(H, T, Sm, Lg).
partition(_, [], [], []).

:-dynamic memo_fib/2.
fib(N,F):-memo_fib(N,F),!.
fib(N,F):- N>1,
	N1 is N-1,
	N2 is N-2,
	fib(N1,F1),
	fib(N2,F2),
	F is F1+F2,
	assertz(memo_fib(N,F)).
fib(0,1).
fib(1,1).

print_all:-memo_fib(N,F),
	write(N),
	write('-'),
	write(F),
	nl,
	fail.
print_all.

perm(L, [H|R]):-append(A, [H|T], L), append(A, T, L1), perm(L1, R).
perm([], []).

all_perm(L,_):-perm(L,L1),
			assertz(p(L1)),
			fail.
all_perm(_,R):-collect_perms(R).

collect_perms([L1|R]):-retract(p(L1)),
			!,
			collect_perms(R).
collect_perms([]).

%exercises

il_to_dl(L,RE,RE):-var(L),!.
il_to_dl([H|T],[H|RS],RE):-il_to_dl(T,RS,RE).

dl_to_il(LS,LS,_):- var(LS),!. 
dl_to_il([H|T],LE,[H|R]):-dl_to_il(T,LE,R).

cl_to_dl([],RE,RE).
cl_to_dl([H|T],[H|RS],RE):-cl_to_dl(T,RS,RE).

dl_to_cl(LS,LS,[]):-var(LS),!.
dl_to_cl([X|T],L,[X|R]):- dl_to_cl(T,L,R).

all_decompositions(L, _):- 
	append(X, Y, L),
	assertz(dec(X, Y)),
	fail.
all_decompositions(_,R):- collect_decompositions(R).

collect_decompositions(R):-
	retract(dec(X, Y)), !,
	collect_decompositions(R1),
	append([[X, Y]], R1, R).
collect_decompositions([]).

flatten_dl([],RE, RE).
flatten_dl([H|T], [H|RS], RE):-atomic(H),!,flatten_dl(T,RS,RE).
flatten_dl([H|T], RS, RE) :- 
	flatten_dl(H,RS1, RE1), 
	flatten_dl(T,RS2, RE2), 
	RS = RS1,
	RE1 = RS2,
	RE = RE2.
	
even_keys_dl(nil,L,L).
even_keys_dl(t(K,L,R),LS,LE):-
	0 is mod(K, 2)
	,!,
    even_keys_dl(L,LSL,LEL),
    even_keys_dl(R,LSR,LER),
    LS=LSL,
    LEL=[K|LSR],
    LE=LER.
% if key odd
even_keys_dl(t(_,L,R),LS,LE):-
	even_keys_dl(L,LSL,LEL),
    even_keys_dl(R,LSR,LER),
    LS=LSL,
    LEL=LSR,
    LE=LER.
	
keys_between_dl(T,K1,K2,L,L):- var(T).
keys_between_dl(t(K,L,R),K1,K2,LS,LE):-
	K1 < K,
	K2 > K,
	!,
    keys_between_dl(L,K1,K2,LSL,LEL),
    keys_between_dl(R,K1,K2,LSR,LER),
    LS=LSL,
    LEL=[K|LSR],
    LE=LER.

keys_between_dl(t(_,L,R),K1,K2,LS,LE):-
	keys_between_dl(L,K1,K2,LSL,LEL),
    keys_between_dl(R,K1,K2,LSR,LER),
    LS=LSL,
    LEL=LSR,
    LE=LER.