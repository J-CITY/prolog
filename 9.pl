%diff(term, atom, term)
%diff(+, +, +-)


dfdg(F, Y, G, X, R) :-
  diff(G, X, DG),
  diff(F, Y, DF),
  set(DF, Y, G, DCF),
  simplify_mul(DCF, DG, R).

op(+).
op(-).
op(*).
op(/).
op(^).
op(sin).
op(cos).
op(ln).  
op(exp).  
op(tan).  
op(cotan).  
  

set(X, X, G, G).
set(C, _, _, C) :- atomic(C).
set(F, X, G, R) :-
  F =..[OP, U],
  op(OP),
  set(U, X, G, R1),
  R =..[OP, R1].
set(F, X, G, R) :-
  F =..[OP, U, V],
  op(OP),
  set(U, X, G, R1),
  set(V, X, G, R2),
  R =..[OP, R1, R2].
  

%n_n(0).
%n_n(s(X)) :- n_n(X).
%
%plus(0, X, X).
%plus(X, 0 , X).
%plus(s(X), Y, Z) :- plus(X, s(Y), Z).
%
%minus(X,0,X).
%minus(X,X,0).
%minus(s(X),s(Y),Z):-minus(X,Y,Z).
%
%mul(s(0), X, X).
%mul(X, s(0), X).
%mul(0, X, 0).
%mul(X, 0, 0).
%mul(s(X), Y, Z) :- mul(X, Y, Z).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%add
simplify_sum(X, 0, X).
simplify_sum(0, X, X).
simplify_sum(X, Y, Z) :- number(X), number(Y), Z is X+Y.
simplify_sum(X, X, 2*X).
simplify_sum(X, Y, X+Y).
%sub
simplify_sub(X, 0, X).
simplify_sub(0, X, -X).
simplify_sub(X, Y, Z) :- number(X), number(Y), Z is X-Y.
simplify_sub(X, X, 0).
simplify_sub(X, Y, X-Y).
%mul
simplify_mul(0, X, 0).
simplify_mul(X, 0, 0).
simplify_mul(X, 1, X).
simplify_mul(1, X, X).
simplify_mul(X, Y, Z) :- number(X), number(Y), Z is X*Y.
simplify_mul(X, Y, X*Y).
%div
simplify_div(X, 0, inf).
simplify_div(0, X, 0).
simplify_div(X, 1), X).
simplify_div(X, X, 1).
simplify_div(X, Y, X/Y).

simplify_exp(X, 1, X).
simplify_exp(X, 0, 1).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% variable
%diff(X, Y, 0) :- not(number(X)), not(X = Y).
diff(X, X, 1) :- not(number(X)).

% constant
diff(C, X, 0) :- not(number(X)), atomic(C).

% sum f+g=f'+g'
diff(X + Y, D, Z) :- diff(X, D, DX), diff(Y, D, DY), simplify_sum(DX, DY, Z).

% subtraction f-g=f'-g'
diff(X - Y, D, Z) :- diff(X, D, DX), diff(Y, D, DY), simplify_sub(DX, DY, Z).
	
% (fg)' = f'g + fg'
diff(X * Y, D, Z) :- diff(X, D, DX), diff(Y, D, DY), simplify_mul(DX, Y, Z0), 
	simplify_mul(X, DY, Z1), simplify_sum(Z0, Z1, Z).

% (f/g)' = (f'g - fg') / (g*g)
diff(X / Y, D, Z) :- diff(X, D, DX), diff(Y, D, DY),
	simplify_mul(DX, Y, A),	simplify_mul(X, DY, B),
	simplify_mul(Y, Y, C), simplify_sub(A, B, D_),
	simplify_div(D_, C, Z).

% functions
% (h(g))' = h'(g) * g'
diff(sin(X), D, Z) :- diff(X, D, DX), simplify_mul(DX, cos(X), Z).

diff(cos(X), D, Z) :- diff(X, D, DX), simplify_mul(DX, sin(X), Z0), simplify_sub(0, Z0, Z).
	
diff(tan(X), D, Z) :- diff(X, D, DX),
	simplify_mul(cos(X), cos(X), A), simplify_div(1, A, B), simplify_mul(DX, B, Z).

diff(cotan(X), D, Z) :- diff(X, D, DX),
	simplify_mul(sin(X), sin(X), A), simplify_div(1, A, B), simplify_mul(DX, B, C), simplify_sub(0, C, Z).
	
diff(exp(X), D, Z) :- diff(X, D, DZ), simplify_mul(DX, exp(X), Z).

diff(log(X), D, Z) :- diff(X, D, DX), simplify_div(DX, X, Z).
	
% (f^g)' = f^(g-1)*(gf' + g'f logf) 
diff(F^G, D, Z) :- diff(F, D, DF), diff(G, D, DG),
	simplify_sub(G, 1, A), writeln(A), simplify_mul(G, DF, B), writeln(B), simplify_mul(DG, F, C), writeln(C),
	simplify_mul(C, log(F), D_), writeln(D_), simplify_sum(B, D_, E), writeln(E), simplify_mul(F^(A), E, Z).
	
	
test1() :-
  dfdg(x, x, y, y, 1).

test2() :-
  dfdg(1, x, y, y, 0).

test3() :-
  dfdg(x + x, x, y, y, 2).

test4() :-
  dfdg(x + x^2, x, y, y, 1+2*x).

test5() :-
  dfdg(x + 3*x^2, x, y, y, 1+6*x).

test6() :-
  dfdg(1 + x + 3*x^2, x, y, y, 1+6*x).

test7() :-
  dfdg(1 + x + 3*x^2, y, y, y, 0).

test8() :-
  dfdg(ln(x), x, 2*y, y, 1/y).

test9() :-
  dfdg(2*ln(x), x, 2*y, y, 2/y).

test10() :-
  dfdg(sin(x), x, y, y, cos(y)).

test11() :-
  dfdg(cos(x), x, y, y, -sin(y)).

test12() :-
  dfdg(ln(x), x, sin(y), y, cos(y)/sin(y)).

test13() :-
  dfdg(sin(x), x, cos(y), y, -((cos(cos(y)))*sin(y))).

test14() :-
  dfdg(sin(x), x, cos(y), y, Res),
  Res = -((cos(cos(y)))*sin(y)).
	