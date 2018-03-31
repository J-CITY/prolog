
bool(true).
bool(false).



isTrue(true).
isFalse(false).

bexpr(X, X) :- bool(X), atom(X).

bexpr(!X, true) :- bool(X), bexpr(X, _X), isFalse(_X).
bexpr(!X, false) :- bool(X), bexpr(X, _X), isTrue(_X).

bexpr(X^Y, true) :- bool(X), bool(Y), bexpr(X, _X), bexpr(Y, _Y), isTrue(_X), isTrue(_Y). 
bexpr(X^Y, false) :- bool(X), bool(Y), bexpr(X, _X), bexpr(Y, _Y), isTrue(_X), isFalse(_Y). 
bexpr(X^Y, false) :- bool(X), bool(Y), bexpr(X, _X), bexpr(Y, _Y), isFalse(_X), isTrue(_Y). 
bexpr(X^Y, false) :- bool(X), bool(Y), bexpr(X, _X), bexpr(Y, _Y), isFalse(_X), isFalse(_Y). 

bexpr(X|Y, true) :- bool(X), bool(Y), bexpr(X, _X), bexpr(Y, _Y), isTrue(_X), isTrue(_Y). 
bexpr(X|Y, true) :- bool(X), bool(Y), bexpr(X, _X), bexpr(Y, _Y), isTrue(_X), isFalse(_Y). 
bexpr(X|Y, true) :- bool(X), bool(Y), bexpr(X, _X), bexpr(Y, _Y), isFalse(_X), isTrue(_Y). 
bexpr(X|Y, false) :- bool(X), bool(Y), bexpr(X, _X), bexpr(Y, _Y), isFalse(_X), isFalse(_Y). 


appendlist([], X, X).
appendlist([T|H], X, [T|L]) :- appendlist(H, X, L).

permutation([], []).
permutation([X], [X]) :-!.
permutation([T|H], X) :- permutation(H, H1), appendlist(L1, L2, H1), appendlist(L1, [T], X1), appendlist(X1, L2, X).




and(A, B) :- A, B.

evaluate(E, true) :- E, !.
evaluate(_, false).

tableBody(A,B,E) :-
  bool(A),
  bool(B),
  write(A),
  write(' \t '),
  write(B),
  write(' \t '),
  evaluate(E, Result),
  write(Result),nl, fail.


