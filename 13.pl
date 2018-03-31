%position(term, term, list)
%position(+-, +-, +-)
%
position(Sub,Term,Res) :- findall(Pos, pos(Sub, Term, Pos), Res).

pos(Term, Term, []).
pos(Sub, Term, Pos) :- compound(Term), functor(Term, F, N), pos(N, Sub, Term, Pos).
pos(N, Sub, Term, [N|Pos]) :- arg(N, Term, Arg), pos(Sub, Arg, Pos).
pos(N, Sub, Term, Pos) :- N > 1, N1 is N-1, pos(N1, Sub, Term, Pos).

%pos(Term, Term, [;]).
%pos1(Term, Term1, []).
%pos(Sub, Term, Pos) :- compound(Term), functor(Term, F, N), pos(N, Sub, Term, Pos).
%pos(N, Sub, Term, [N|Res]) :- N > 0, arg(N, Term, Arg), pos(Sub, Arg, Pos), N1 is N-1, pos(N1, Sub, Term, T),
%	append(Pos, T, Res).
%pos(N, Sub, Term, Pos) :- N > 0, arg(N, Term, Arg), pos1(Sub, Arg, []), N1 is N-1, pos(N1, Sub, Term, Pos).
%pos(0, Sub, Term, Pos).

test0() :- pos(x, e, []).
test1() :- pos(x, s(x), [1]).
test2() :- pos(x, s(y, x), [2]).
test3() :- pos(x, s(x, y), [[2]]).
test4() :- pos(x, a(s(z, 2), d(3, r(y,x,z))), [2,2,2]).

test5() :- pos(X, a(x,2), [1]), X = x.

test6() :- pos(X, a(x,2), Y), X=a(x,2), Y=[].
test7() :- pos(X, a(x,2), Y), X=2, Y=[2].
test8() :- pos(X, a(x,2), Y), X=x, Y=[1].

test8() :- pos(x, a(x,2), Y), Y=[1].

test9() :- pos(x, X, Y), Y=[], X=x.

test10() :- pos(X, Y, Z), X=Y, Z=[].



test() :- test0(),test1(),test2(),test3(),test4().

