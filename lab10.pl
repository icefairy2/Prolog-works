neighbor(a, [b, d]). % an example graph – 1st connected component of the
neighbor(b, [a, c, d]). % example graph
neighbor(c, [b, d]).

neighb_to_edge:-neighbor(Node,List),
	process(Node,List),
	fail.
neighb_to_edge.

process(Node, [H|T]):- assertz(edge(Node, H)),
	process(Node, T).
process(_, []).

path(X,Y,Path):-path(X,Y,[X],Path).
path(X,Y,PPath, FPath):- arc(X,Z),
	\+(member(Z, PPath)),
	path(Z, Y, [Z|PPath], FPath).
path(X,X,PPath, PPath).

is_edge(X,Y):- edge(X,Y); edge(Y,X).

edge(a,b).
edge(a,d).
edge(b,d).
edge(b,c).
edge(c,d).

% restricted_path(Source, Target, RestrictionsList, Path)
% check_restrictions(Lrestrictions, Path)
restricted_path(X,Y,LR,P):- path(X,Y,P),
		check_restrictions(LR, P).
		
check_restrictions([],_):- !.
check_restrictions([H|T], [H|R]):- !, check_restrictions(T,R).
check_restrictions(T, [H|L]):-check_restrictions(T,L).

%optimal_path(Source, Target, Path)
optimal_path(X,Y,Path):-asserta(sol_part([],100)),
			path(X,Y,[X],Path,1).
optimal_path(_,_,Path):-retract(sol_part(Path,_)).
path(X,X,Path,Path,LPath):-retract(sol_part(_,_)),!,
		asserta(sol_part(Path,LPath)),
		fail.
path(X,Y,PPath,FPath,LPath):-edge(X,Z),
		\+(member(Z,PPath)),
		LPath1 is LPath+1,
		sol_part(_,Lopt),
		LPath1<Lopt,
		path(Z,Y,[Z|PPath],FPath,LPath1).
		
%hamilton(NbNodes, Source, Path)
hamilton(NN, X, Path):- NN1 is NN-1, hamilton_path(NN1,X, X, [X],Path).

%exercises

:- dynamic neighbour/2.

neighbour(a, []). 
neighbour(b, []).
neighbour(c, []).
neighbour(d, []).

edge_to_neigh_list:-
	retract(edge(X, Y)),
	process_node(X, Y),
	process_node(Y, X),
	edge_to_neigh_list.
edge_to_neigh_list.

process_node(Node, Neighbour):-
	retract(neighbour(Node, L)), 
	assertz(neighbour(Node,[Neighbour|L])).
	
%optimal_path1(Source, Target, Path)
optimal_path1(X,Y,Path):-
	asserta(sol_part([],100)),
	path1(X,Y,[X],Path,1).
optimal_path1(_,_,Path):-
	retract(sol_part(Path,_)).
path1(X,X,Path,Path,LPath):-
	retract(sol_part(_,_)),!,
	asserta(sol_part(Path,LPath)),
	fail.
path1(X,Y,PPath,FPath,LPath):-
	arc_w(X,Z,Weight),
	\+(member(Z,PPath)),
	LPath1 is LPath+Weight,
	sol_part(_,Lopt),
	LPath1<Lopt,
	path1(Z,Y,[Z|PPath],FPath,LPath1).