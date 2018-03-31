
bool(t).
bool(f).

isTrue(t).
isFalse(f).

be(F, L, X) :- bexpr(F, X).

bexpr(X, X) :- bool(X), atom(X).

bexpr(~(X), t) :- bool(X), bexpr(X, _X), isFalse(_X).
bexpr(~(X), f) :- bool(X), bexpr(X, _X), isTrue(_X).

bexpr(X^Y, t) :- bexpr(X, _X), bexpr(Y, _Y), isTrue(_X), isTrue(_Y). 
bexpr(X^Y, f) :- bexpr(X, _X), bexpr(Y, _Y), isTrue(_X), isFalse(_Y). 
bexpr(X^Y, f) :- bexpr(X, _X), bexpr(Y, _Y), isFalse(_X), isTrue(_Y). 
bexpr(X^Y, f) :- bexpr(X, _X), bexpr(Y, _Y), isFalse(_X), isFalse(_Y). 

bexpr(X|Y, t) :- bexpr(X, _X), bexpr(Y, _Y), isTrue(_X), isTrue(_Y). 
bexpr(X|Y, t) :- bexpr(X, _X), bexpr(Y, _Y), isTrue(_X), isFalse(_Y). 
bexpr(X|Y, t) :- bexpr(X, _X), bexpr(Y, _Y), isFalse(_X), isTrue(_Y). 
bexpr(X|Y, f) :- bexpr(X, _X), bexpr(Y, _Y), isFalse(_X), isFalse(_Y). 



%sknf(Fun, Perem, SKNF) :- findall(L, bexpr(Fun, L), SKNF).
sknf(Fun, Perem, SKNF) :- findall(L, genAll(Fun, Perem, L), SKNF).

genAll(FUN, 0, []).
genAll(FUN, N, [H|T]) :- N > 0, N1 is N-1, bool(H), genAll(FUN, N1, T).



%sknf(F, SKNF) :- findall(E, bexpr(F, E), SKNF).
