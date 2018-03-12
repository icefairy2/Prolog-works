%member(X, [H|T]):- H=X.
member1(X, [X|_]).
member1(X, [_|T]):-member1(X,T).

append1([], L, L).
append1([H|T], L, [H|R]):-append1(T, L, R).

delete1(X, [X|T], T).
%traverse the list, add the elements H≠X back to the result
delete1(X, [H|T], [H|R]):-delete1(X, T, R).
delete1(_, [], []).

delete_all(X, [X|T], R):-delete_all(X, T, R).
delete_all(X, [H|T], [H|R]):-delete_all(X, T, R).
delete_all(_, [], []).

%exercises

append3(L1, L2, L3, R):-
	append1(L1, L2, R1),
	append1(R1, L3, R).
	
add_element(X, L, [X|L]).

sum_int([], 0).
sum_int([H|T], R):- sum_int(T, R1), R is H+R1.

separate_parity([], [], []).
separate_parity([H|T], [H|E], O):-
	H mod 2 =:= 0, 
	separate_parity(T, E, O).
separate_parity([H|T], E, [H|O]):-
	H mod 2 =:= 1, 
	separate_parity(T, E, O).
	
remove_dup([],[]).
remove_dup([H|T], R) :- member(H,T), remove_dup(T,R).
remove_dup([H|T], [H|R]):-remove_dup(T,R).

replace_all(_, _, [], []).
replace_all(K, NewK, [K|T], [NewK|R]):- replace_all(K, NewK, T, R).  
replace_all(K, NewK, [H|T], [H|R]):- replace_all(K, NewK, T, R).  

drop_k([], _, []).
drop_k([_|T], 1, T).
drop_k([H|T], K, [H|R]) :- K>0, K1 is K-1, drop_k(T, K1, R), K is K+1.

delete_nth(L,C,R) :-
    delete_nth(L,C,1,R).

delete_nth([],_,_,[]).
delete_nth([_|T],C,C,T1) :- !, delete_nth(T,C,1,T1).
delete_nth([H|T],N,C,[H|T1]) :- C<N, C1 is C+1, delete_nth(T,N,C1,T1).