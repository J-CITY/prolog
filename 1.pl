% No 1 fib(N, F) - n Fibonacci number

fib(0, 1).
fib(1, 1).
fib(N, F) :-  N > 1, plus(N1, 1, N), plus(N2, 2, N), fib(N1, F1), fib(N2, F2), plus(F1, F2, F).

% Tests

test0(fib) :- fib(0, X), X=:=1.     % expect 1
test1(fib) :- fib(1, X), X=:=1.     % expect 1
test2(fib) :- fib(7, X), X=:=21.    % expect 21
test3(fib) :- fib(7, 21).           % expect true 
test4(fib) :- not(fib(7, 2)).       % expect false 

test(fib) :- test0(fib), test1(fib), test2(fib), test3(fib), test4(fib).
