gcd(X, X, X).
gcd(X, Y, Z):- X>Y, R is X-Y, gcd(R, Y, Z).
gcd(X, Y, Z):- X<Y, R is Y-X, gcd(X, R, Z).

fact(0, 1).
fact(N, F):-N>0, N1 is N-1, fact(N1, F1), F is F1*N. 

fact1(0, FF, FF).
fact1(N, FP, FF):-N>0, N1 is N-1, FP1 is FP*N, fact1(N1, FP1, FF).
fact1_pretty(N,F):-fact1(N,1,F).

sum(A, I, Res):- Res is A+I.
for(In,In,0).
for(In,Out,I):-
	I>0,
	NewI is I-1,
	sum(In, I, Intermediate),
	for(Intermediate,Out,NewI).
	
%exercises
	
lcm(X, X, X).
lcm(X, Y, Z):- Prod is X*Y, gcd(X, Y, Gcd), Z is Prod / Gcd.

fib(0, 1).
fib(1, 1).
fib(N, R) :- N>1, N1 is N-1, N2 is N-2, fib(N1, R1), fib(N2, R2), R is R1 + R2.
	
repeatUntil(End, End) :- write(End).
repeatUntil(Start, End):-
	NewStart is Start+1,
	write(Start),
	repeatUntil(NewStart, End).
	
while(Start, End) :-
	Start =< End,
	NewStart is Start+1,
	write(Start),
	while(NewStart, End).

triangle(A, B, C):-
	X is A+B, X>C, 
	Y is C+B, Y>A,
	Z is A+C, Z>B.
	
solve_eq2(A, B, C, X):- 
  D is B*B-4*A*C,
  D < 0,
  X = 'complex roots'.
  
solve_eq2(A, B, C, R):-
	Delta is B*B - 4*A*C,
	Delta >= 0,
	R is (-B - sqrt(Delta))/2*A.

solve_eq2(A, B, C, R):-
	Delta is B*B - 4*A*C,
	Delta >= 0,
	R is (-B + sqrt(Delta))/2*A.	
