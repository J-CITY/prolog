% No 3 mergesort(L, SL) - merge sort

merge(L, [], L).
merge([], L, L).
merge([HA|TA], [HB|TB], LH) :- HA =< HB, merge(TA, [HB|TB], LT), LH = [HA|LT].
merge([HA|TA], [HB|TB], LH) :- HA >  HB, merge(TB, [HA|TA], LT), LH = [HB|LT].

split([], [], []).
split([H|T1], E, [H|T2]) :- split(T1, T2, E).

mergesort([],[]).
mergesort([X],[X]).
mergesort(L,SL):-
    L = [_,_|_],
	split(L, L1, L2),
	mergesort(L1, SL1), 
	mergesort(L2, SL2),
	merge(SL1, SL2, SL).

% Tests

equallist(sl,sl).
equallist([A|L],L1):-del(A,L1,L2), equallist(L,L2).
equallist([], []).
del(symbol,sl,sl).
del(A,[A|L],L):- !.
del(A,[A|L],[A|L1]):- del(A,L,L1).

% Tests

test0(mergesort) :- mergesort([], X),  equallist(X, []).
test1(mergesort) :- mergesort([1], X), equallist(X, [1]).
test2(mergesort) :- mergesort([1, 5, -1, 3], X), equallist(X, [-1, 1, 3, 5]).
test3(mergesort) :- mergesort([1, 5, -1, 3, 7], X), equallist(X, [-1, 1, 3, 5, 7]).

test4(mergesort) :- mergesort([1, 5, -1, 3, 7], [-1, 1, 3, 5, 7]).
test5(mergesort) :- not(mergesort([1, 5, -1, 3, 7], [-1, 1, 3, 5])).
test6(mergesort) :- mergesort([1, 5, -1, 3], [-1, 1, 3, 5]).
test7(mergesort) :- not(mergesort([1, 5, -1, 3], [-1, 1, 3, 6])).

test(mergesort) :-
	test0(mergesort),
	test1(mergesort),
	test2(mergesort),
	test3(mergesort),
	test4(mergesort),
	test5(mergesort),
	test6(mergesort),
	test7(mergesort).
