

dfdg(F, Y, G, X, R) :-
  ds(G, X, DG),
  ds(F, Y, DF),
  set(DF, Y, G, DCF),
  simplify(DCF * DG, R).

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
  

n_n(0).
n_n(s(X)) :- n_n(X).

plus(0, X, X).
plus(X, 0 , X).
plus(s(X), Y, Z) :- plus(X, s(Y), Z).

minus(X,0,X).
minus(X,X,0).
minus(s(X),s(Y),Z):-minus(X,Y,Z).

mul(s(0), X, X).
mul(X, s(0), X).
mul(0, X, 0).
mul(X, 0, 0).
mul(s(X), Y, Z) :- mul(X, Y, Z).

d(X, X, s(0)) :- not(n_n(X)). 
d(C, X, 0) :- n_n(C). 

d(-U, X, -A) :- d(U, X, A). % (-u)’=-u’
d(U+V, X, A+B) :- d(U,X,A), d(V,X,B). % (u+v)’=u’+v’.
d(U-V, X, A-B) :- d(U,X,A), d(V,X,B). 

d(C*U, X, C*A) :- n_n(C), d(U, X, A).

d(U*V, X, U*B+A*V) :- d(U, X, A), d(V, X, B). % (uv)’=uv’+u’v

d(U/V, X, (A*V-U*B)/V^2) :- d(U, X, A), d(V, X, B).

d(U^C, X, C*A*U^(C-1)) :- n_n(C), d(U, X, A).

d(sin(C), X, 0) :- n_n(C).

d(cos(C), X, 0) :- n_n(C).

d(sin(X), X, cos(X)).

d(cos(X), X, -sin(X)).


d(sin(G), X, cos(G)*DG) :- d(G, X, DG).
d(cos(G), X, -sin(G)*DG) :- d(G, X, DG).

d(exp(G), X, exp(G)*DG ) :- d(G, X, DG). 
d( log(G), X, DG/G ):- d(G, X, DG).  

simplify(C, C) :- n_n(C).
simplify(X, X) :- atom(X).

simplify(X*0, 0).
simplify(0*X, 0).

simplify(X+0, SX) :- simplify(X, SX).
simplify(0+X, SX) :- simplify(X, SX).

simplify(X-0, X).
simplify(0-X, -X).

simplify(A+B, AB) :- n_n(A), n_n(B), plus(A, B, AB).
simplify(A-B, AB) :- n_n(A), n_n(B), minus(A, B, AB).

simplify(X*s(0), SX) :- simplify(X, SX).
simplify(s(0)*X, SX) :- simplify(X, SX).

simplify(A*B, AB) :- n_n(A), n_n(B), mul(A, B, AB).


simplify(A*F, A*SF) :- n_n(A), simplify(F, SF).
simplify(F*A, A*SF) :- n_n(A), simplify(F, SF).

simplify(A*F+B*F, AB*SF) :- n_n(A), n_n(B), plus(A, B, AB), simplify(F, SF).

simplify(X+Y, X1+Y1) :- simplify(X, X1), simplify(Y, Y1).
simplify(X*Y, X1*Y1) :- simplify(X, X1), simplify(Y, Y1).
simplify(X/Y, X1/Y1) :- simplify(X, X1), simplify(Y, Y1).
simplify(X-Y, X1-Y1) :- simplify(X, X1), simplify(Y, Y1).



ds(F, X, RES) :- d(F, X, R), simplify(R, R1), simplify(R1, RES).

% Tests

test0() :- ds(s(s(s(0)))*x*x+s(s(0))*x + s(0), x,  s(s(s(s(s(s(0))))))*x+s(s(0))).
test1() :- ds(s(0), x,  0).
test1() :- ds(s, x,  s(0)).
test2() :- ds(s(s(0))*x, x,  s(s(0))).
test3() :- ds(s(s(0))*x + s(0), x,  s(s(0))).
test4() :- ds(0*x, x,  0).
test5() :- ds(x*0, x,  0).

test6() :- ds(x+0, x,  s(0)).
test7() :- ds(x+0, x,  s(0)).

test8() :- ds(x*s(0), x,  s(0)).
test9() :- ds(x*s(0), x,  s(0)).

test10() :- ds(x*s(0), x,  X), X=s(0).
test10() :- ds(x*s(0), x,  X), X=s(0).

%test(ds) :- test0(ds),test1(ds),test2(ds),test3(ds),test4(ds),test5(ds),test6(ds),test7(ds),test8(ds),test9(ds).










