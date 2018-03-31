%occurrences(term, term, number)
%occurrences(+-, +-, +-)

occurrences(Sub, Term, R) :- occurrences(Sub, Term, 0, R).

occurrences(Sub, Sub, Acc, R) :- R is Acc + 1.

occurrences(Sub, Term, Acc, R) :- compound(Term), functor(Term, F, N), occurrences(N, Sub, Term, Acc, R).

occurrences(Sub, Term, Acc, Acc) :- not(compound(Term)).

occurrences(N, Sub, Term, Acc, R4) :- N > 0, arg(N,Term, NA), occurrences(Sub, NA, Acc, R2),
                               N1 is N-1, occurrences(N1, Sub, Term, R2,R4). 

occurrences(N,_,_,Acc,Acc) :- (N = 0).


%tests
test0() :- occurrences(x, 1, 0).
test1() :- occurrences(x, x, 1).
test2() :- occurrences(x, 2*x, 1).
test3() :- occurrences(2*x, sin(2*x)+x+2*x+3*x, 2).
test4() :- occurrences(x, sin(2*x)+x+2*x+3*x, 4).

test() :- test0(), test1(), test2(), test3(), test4().

