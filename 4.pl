% No 4 Poker
%successor(royal_flush, straight_flush).   
%successor(straigh_flush, four_of_a_kind).
%successor(four_of_a_kind, full_house).    
%successor(full_house, flush).
%successor(flush, straight).               
%successor(straight, three_of_a_kind).
%successor(three_of_a_kind, two_pair).     
%successor(two_pair, pair).
%successor(pair, high_card).
%successor(ace, king).     
%successor(king, queen).   
%successor(queen, jack).
%successor(jack, 10).      
%successor(10, 9).         
%successor(9, 8).
%successor(8, 7).          
%successor(7, 6).          
%successor(6, 5).
%successor(5, 4).          
%successor(4, 3).          
%successor(3, 2).
%successor(clubs, spades).
%successor(spades, hearts).
%successor(hearts, diamonds).

%royal_flush
test0() :- winner([[10, clubs],[jack, clubs],[queen, clubs],[king, clubs],[ace, clubs]], 
	              [[10, hearts],[jack, hearts],[queen, hearts],[king, hearts],[ace, hearts]], 1).
%straigh_flush
test1() :- winner([[10, clubs],[jack, clubs],[queen, clubs],[king, clubs],[ace, clubs]], 
                  [[10, hearts],[jack, hearts],[queen, hearts],[king, hearts],[king, hearts]], 1).
%straigh_flush
test2() :- winner([[2, clubs],[3, hearts],[4, clubs],[5, clubs],[6, clubs]], 
                  [[10, hearts],[jack, hearts],[queen, hearts],[king, hearts],[king, hearts]], 2).
				  
%four_of_a_kind
test3() :- winner([[2, clubs],[2, spades],[2, hearts],[2, diamonds],[6, clubs]], 
                  [[jack, clubs],[jack, spades],[jack, hearts],[jack, diamonds],[king, clubs]], 2).
%full_house
test4() :- winner([[jack, clubs],[jack, spades],[jack, hearts],[2, diamonds],[2, clubs]], 
                  [[10, clubs],[10, spades],[10, hearts],[6, diamonds],[6, clubs]], 1).
%flush
test5() :- winner([[jack, clubs],[2, clubs],[5, clubs],[6, clubs],[10, clubs]], 
                  [[jack, diamonds],[2, diamonds],[5, diamonds],[6, diamonds],[10, diamonds]], 1).
%straight
test6() :- winner([[10, clubs],[jack, hearts],[queen, clubs],[king, clubs],[ace, clubs]], 
                  [[10, hearts],[jack, hearts],[queen, hearts],[king, hearts],[king, clubs]], 1).
%straigh
test7() :- winner([[2, clubs],[3, hearts],[4, clubs],[5, clubs],[6, clubs]], 
                  [[10, hearts],[jack, hearts],[queen, hearts],[king, hearts],[king, clubs]], 2).
%three_of_a_kind
test8() :- winner([[2, clubs],[2, hearts],[2, diamonds],[5, clubs],[6, clubs]], 
                  [[3, clubs],[3, hearts],[3, diamonds],[5, clubs],[6, clubs]], 2).
%three_of_a_kind
test9() :- winner([[2, clubs],[2, hearts],[2, diamonds],[5, clubs],[6, clubs]], 
                  [[4, clubs],[3, hearts],[ace, diamonds],[ace, clubs],[ace, hearts]], 2).
%three_of_a_kind
test10() :- winner([[2, clubs],[2, hearts],[2, diamonds],[5, clubs],[6, clubs]], 
                   [[4, clubs],[king, hearts],[king, diamonds],[king, clubs],[ace, clubs]], 2).
%two_pair
test11() :- winner([[2, clubs],[2, hearts],[3, diamonds],[3, clubs],[6, clubs]], 
                   [[3, clubs],[3, hearts],[4, diamonds],[4, clubs],[8, clubs]], 2).

test12() :- winner([[2, clubs],[2, hearts],[3, diamonds],[3, clubs],[6, clubs]], 
                   [[2, clubs],[2, hearts],[6, diamonds],[7, clubs],[7, clubs]], 2).

test13() :- winner([[2, clubs],[2, hearts],[3, diamonds],[6, diamonds],[6, clubs]], 
                   [[2, clubs],[2, hearts],[6, diamonds],[6, clubs],[8, hearts]], 2).

test14() :- winner([[2, clubs],[3, hearts],[3, diamonds],[6, clubs],[6, clubs]], 
                   [[3, clubs],[3, hearts],[4, diamonds],[8, diamonds],[8, clubs]], 2).

test15() :- winner([[2, clubs],[3, hearts],[3, diamonds],[6, clubs],[6, clubs]], 
                   [[3, clubs],[6, hearts],[6, diamonds],[8, diamonds],[8, clubs]], 2).

%pair
test16() :- winner([[2, clubs],[2, hearts],[3, diamonds],[5, clubs],[6, clubs]], 
                   [[3, clubs],[3, hearts],[4, diamonds],[5, clubs],[8, clubs]], 2).

test17() :- winner([[2, clubs],[2, hearts],[3, diamonds],[5, clubs],[6, clubs]], 
                   [[3, clubs],[4, hearts],[6, diamonds],[6, clubs],[8, clubs]], 2).

test18() :- winner([[2, clubs],[2, hearts],[3, diamonds],[5, clubs],[6, clubs]], 
                   [[3, clubs],[5, hearts],[6, diamonds],[8, clubs],[8, hearts]], 2).

test19() :- winner([[2, clubs],[3, hearts],[3, diamonds],[5, clubs],[6, clubs]], 
                   [[3, clubs],[3, hearts],[4, diamonds],[5, clubs],[8, clubs]], 2).

test20() :- winner([[2, clubs],[3, hearts],[3, diamonds],[5, clubs],[6, clubs]], 
                   [[3, clubs],[4, hearts],[6, diamonds],[6, clubs],[8, clubs]], 2).

test21() :- winner([[2, clubs],[3, hearts],[3, diamonds],[5, clubs],[6, clubs]], 
                   [[3, clubs],[5, hearts],[6, diamonds],[8, clubs],[8, hearts]], 2).
				   
				   
test22() :- winner([[2, clubs],[4, hearts],[5, diamonds],[8, clubs],[king, clubs]], 
                   [[3, clubs],[5, hearts],[6, diamonds],[8, clubs],[ace, hearts]], 2).
			
test23() :- winner([[2, clubs],[4, hearts],[5, diamonds],[8, clubs],[king, clubs]], 
                   [[2, clubs],[4, hearts],[5, diamonds],[8, clubs],[king, diamonds]], 1).
		 
test24() :- winner([[10, clubs],[jack, clubs],[queen, clubs],[king, clubs],[ace, clubs]], 
	               [[10, clubs],[jack, clubs],[queen, clubs],[king, clubs],[ace, clubs]], 1).

test25() :- winner([[2, clubs],[2, spades],[2, hearts],[2, diamonds],[6, clubs]], 
                  [[10, clubs],[jack, hearts],[queen, clubs],[king, clubs],[ace, clubs]], 1).

test26() :- winner([[jack, clubs],[2, clubs],[5, clubs],[6, clubs],[10, clubs]], 
                  [[3, clubs],[3, hearts],[4, diamonds],[4, clubs],[8, clubs]], 1).

test27() :- winner([[2, clubs],[3, hearts],[3, diamonds],[5, clubs],[6, clubs]], 
                   [[2, clubs],[3, hearts],[5, diamonds],[7, clubs],[ace, clubs]], 1).				  

test28() :- sortHand([[10, clubs],[10, hearts],[queen, clubs],[king, clubs],[ace, clubs]],
					  [[10, hearts], [10, clubs], [queen, clubs], [king, clubs], [ace, clubs]]).
test29() :- sortHand([[10, hearts],[jack, hearts],[queen, hearts],[king, hearts],[king, clubs]],
					  [[10, hearts], [jack, hearts], [queen, hearts], [king, hearts], [king, clubs]]).
					  
test() :-
test0(),
test1(),
test2(),
test3(),
test4(),
test5(),
test6(),
test7(),
test8(),
test9(),
test10(),
test11(),
test12(),
test13(),
test14(),
test15(),
test16(),
test17(),
test18(),
test19(),
test20(),
test21(),
test22(),
test23(),
test24(),
test25(),
test26(),
test27(),
test28(),
test29().

cmp([[2, clubs],[3, hearts],[3, diamonds],[5, clubs],[6, clubs]], [[2, clubs],[3, hearts],[5, diamonds],[7, clubs],[ace, clubs]],X).

winner(H1, H2, 1) :-
  sortHand(H1, SH1),
  sortHand(H2, SH2),
  getCombination(SH1,  X1),
  getCombination(SH2,  X2),
  cmp(X1, X2, X1).
  
winner(H1, H2, 2) :-
  sortHand(H1, SH1),
  sortHand(H2, SH2),
  getCombination(SH1,  X1),
  getCombination(SH2,  X2),
  cmp(X1, X2, X2).

winner(H1, H2, R) :-
  sortHand(H1, SH1),
  sortHand(H2, SH2),
  getCombination(SH1,  X1),
  getCombination(SH2,  X2),
  (X1 = X2),
  cmpWin(X1, X2, SH1, SH2, R).


% cmps

cmpWin(royal_flush, royal_flush, [H1|T1], [H2|T2], 1) :- cmp(H1, H2, H1).
cmpWin(royal_flush, royal_flush, [H1|T1], [H2|T2], 2) :- cmp(H1, H2, H2).

cmpWin(straight_flush, straight_flush, [H1|T1], [H2|T2], 1) :- cmp(H1, H2, H1).
cmpWin(straight_flush, straight_flush, [H1|T1], [H2|T2], 2) :- cmp(H1, H2, H2).

cmpWin(four_of_a_kind, four_of_a_kind, [H1, HH1|T1], [H2, HH2|T2], 1) :- cmp(HH1, HH2, HH1).
cmpWin(four_of_a_kind, four_of_a_kind, [H1, HH1|T1], [H2, HH2|T2], 2) :- cmp(HH1, HH2, HH2).

cmpWin(full_house, full_house, [H1, HH1, HHH1|T1], [H2, HH2, HHH2|T2], 1) :- cmp(HHH1, HHH2, HHH1).
cmpWin(full_house, full_house, [H1, HH1, HHH1|T1], [H2, HH2, HHH2|T2], 2) :- cmp(HHH1, HHH2, HHH2).

cmpWin(flush, flush, [[V1, S1]|T1], [[V2, S2]|T2], 1) :- cmp(S1, S2, S1).
cmpWin(flush, flush, [[V1, S1]|T1], [[V2, S2]|T2], 2) :- cmp(S1, S2, S2).

cmpWin(straight, straight, [H1,H2,H3,H4,H5], [G1,G2,G3,G4,G5], 1) :- cmp(H5, G5, H5).
cmpWin(straight, straight, [H1,H2,H3,H4,H5], [G1,G2,G3,G4,G5], 2) :- cmp(H5, G5, G5).

cmpWin(three_of_a_kind, three_of_a_kind, [H1,H2,H3,H4,H5], [G1,G2,G3,G4,G5], 1) :- cmp(H3, G3, H3).
cmpWin(three_of_a_kind, three_of_a_kind, [H1,H2,H3,H4,H5], [G1,G2,G3,G4,G5], 2) :- cmp(H3, G3, G3).
% two pair
cmpWin(two_pair, two_pair, [[W, S11],[W, S12],[Q, S13],[Q, S14],[V15, S15]],
	[[E, S21],[E, S22],[R, S23],[R, S24],[V25, S25]], 1) :- 
	cmp([W, S12], [Q, S14], WQ), cmp([E, S22],[R, S24], ER), cmp(WQ, ER, WQ).
cmpWin(two_pair, two_pair, [[W, S11],[W, S12],[Q, S13],[Q, S14],[V15, S15]],
	[[E, S21],[E, S22],[R, S23],[R, S24],[V25, S25]], 2) :- 
	cmp([W, S12], [Q, S14], WQ), cmp([E, S22],[R, S24], ER), cmp(WQ, ER, ER).
	
cmpWin(two_pair, two_pair, [[W, S11],[W, S12],[V13, S13],[Q, S14],[Q, S15]],
	[[E, S21],[E, S22],[R, S23],[R, S24],[V25, S25]], 1) :-
	cmp([W, S12], [Q, S15], WQ), cmp([E, S22],[R, S24], ER), cmp(WQ, ER, WQ).
cmpWin(two_pair, two_pair, [[W, S11],[W, S12],[V13, S13],[Q, S14],[Q, S15]],
	[[E, S21],[E, S22],[R, S23],[R, S24],[V25, S25]], 2) :-
	cmp([W, S12], [Q, S15], WQ), cmp([E, S22],[R, S24], ER), cmp(WQ, ER, ER).
	
cmpWin(two_pair, two_pair, [[V11, S11],[W, S12],[W, S13],[Q, S14],[Q, S15]],
	[[E, S21],[E, S22],[R, S23],[R, S24],[V25, S25]], 1) :-
	cmp([W, S13], [Q, S15], WQ), cmp([E, S22],[R, S24], ER), cmp(WQ, ER, WQ).
cmpWin(two_pair, two_pair, [[V11, S11],[W, S12],[W, S13],[Q, S14],[Q, S15]],
	[[E, S21],[E, S22],[R, S23],[R, S24],[V25, S25]], 2) :-
	cmp([W, S13], [Q, S15], WQ), cmp([E, S22],[R, S24], ER), cmp(WQ, ER, ER).
	
cmpWin(two_pair, two_pair, [[W, S11],[W, S12],[Q, S13],[Q, S14],[V15, S15]],
	[[E, S21],[E, S22],[V23, S23],[R, S24],[R, S25]], 1) :-
	cmp([W, S12], [Q, S14], WQ), cmp([E, S22],[R, S25], ER), cmp(WQ, ER, WQ).
cmpWin(two_pair, two_pair, [[W, S11],[W, S12],[Q, S13],[Q, S14],[V15, S15]],
	[[E, S21],[E, S22],[V23, S23],[R, S24],[R, S25]], 2) :-
	cmp([W, S12], [Q, S14], WQ), cmp([E, S22],[R, S25], ER), cmp(WQ, ER, ER).
	
cmpWin(two_pair, two_pair, [[W, S11],[W, S12],[V13, S13],[Q, S14],[Q, S15]],
	[[E, S21],[E, S22],[V23, S23],[R, S24],[R, S25]], 1) :-
	cmp([W, S12], [Q, S15], WQ), cmp([E, S22],[R, S25], ER), cmp(WQ, ER, WQ).
cmpWin(two_pair, two_pair, [[W, S11],[W, S12],[V13, S13],[Q, S14],[Q, S15]],
	[[E, S21],[E, S22],[V23, S23],[R, S24],[R, S25]], 2) :-
	cmp([W, S12], [Q, S15], WQ), cmp([E, S22],[R, S25], ER), cmp(WQ, ER, ER).
	
cmpWin(two_pair, two_pair, [[V11, S11],[W, S12],[W, S13],[Q, S14],[Q, S15]],
	[[E, S21],[E, S22],[V23, S23],[R, S24],[R, S25]], 1) :-
	cmp([W, S13], [Q, S15], WQ), cmp([E, S22],[R, S25], ER), cmp(WQ, ER, WQ).
cmpWin(two_pair, two_pair, [[V11, S11],[W, S12],[W, S13],[Q, S14],[Q, S15]],
	[[E, S21],[E, S22],[V23, S23],[R, S24],[R, S25]], 2) :-
	cmp([W, S13], [Q, S15], WQ), cmp([E, S22],[R, S25], ER), cmp(WQ, ER, ER).

cmpWin(two_pair, two_pair, [[W, S11],[W, S12],[Q, S13],[Q, S14],[V15, S15]],
	[[V21, S21],[E, S22],[E, S23],[R, S24],[R, S25]], 1) :-
	cmp([W, S12], [Q, S14], WQ), cmp([E, S23],[R, S25], ER), cmp(WQ, ER, WQ).
cmpWin(two_pair, two_pair, [[W, S11],[W, S12],[Q, S13],[Q, S14],[V15, S15]],
	[[V21, S21],[E, S22],[E, S23],[R, S24],[R, S25]], 2) :-
	cmp([W, S12], [Q, S14], WQ), cmp([E, S23],[R, S25], ER), cmp(WQ, ER, ER).
	
cmpWin(two_pair, two_pair, [[W, S11],[W, S12],[V13, S13],[Q, S14],[Q, S15]],
	[[V21, S21],[E, S22],[E, S23],[R, S24],[R, S25]], 1) :-
	cmp([W, S12], [Q, S15], WQ), cmp([E, S23],[R, S25], ER), cmp(WQ, ER, WQ).
cmpWin(two_pair, two_pair, [[W, S11],[W, S12],[V13, S13],[Q, S14],[Q, S15]],
	[[V21, S21],[E, S22],[E, S23],[R, S24],[R, S25]], 2) :-
	cmp([W, S12], [Q, S15], WQ), cmp([E, S23],[R, S25], ER), cmp(WQ, ER, ER).
	
cmpWin(two_pair, two_pair, [[V11, S11],[W, S12],[W, S13],[Q, S14],[Q, S15]],
	[[V21, S21],[E, S22],[E, S23],[R, S24],[R, S25]], 1) :-
	cmp([W, S13], [Q, S15], WQ), cmp([E, S23],[R, S25], ER), cmp(WQ, ER, WQ).
cmpWin(two_pair, two_pair, [[V11, S11],[W, S12],[W, S13],[Q, S14],[Q, S15]],
	[[V21, S21],[E, S22],[E, S23],[R, S24],[R, S25]], 2) :-
	cmp([W, S13], [Q, S15], WQ), cmp([E, S23],[R, S25], ER), cmp(WQ, ER, ER).	

% pair


cmpWin(pair, pair, [[P, S11],[P, S12],[V13, S13],[V14, S14],[V15, S15]],
	[[Q, S21],[Q, S22],[V23, S23],[V24, S24],[V25, S25]], 1) :- cmp([P, S12], [Q, S22], [P, S12]).
cmpWin(pair, pair, [[P, S11],[P, S12],[V13, S13],[V14, S14],[V15, S15]],
	[[Q, S21],[Q, S22],[V23, S23],[V24, S24],[V25, S25]], 2) :- cmp([P, S12], [Q, S22], [Q, S22]).
	
cmpWin(pair, pair, [[P, S11],[P, S12],[V13, S13],[V14, S14],[V15, S15]],
	[[V21, S21],[Q, S22],[Q, S23],[V24, S24],[V25, S25]], 1) :- cmp([P, S12], [Q, S23], [P, S12]).
cmpWin(pair, pair, [[P, S11],[P, S12],[V13, S13],[V14, S14],[V15, S15]],
	[[V21, S21],[Q, S22],[Q, S23],[V24, S24],[V25, S25]], 2) :- cmp([P, S12], [Q, S23], [Q, S23]).
	
cmpWin(pair, pair, [[P, S11],[P, S12],[V13, S13],[V14, S14],[V15, S15]],
	[[V21, S21],[V22, S22],[Q, S23],[Q, S24],[V25, S25]], 1) :- cmp([P, S12], [Q, S24], [P, S12]).
cmpWin(pair, pair, [[P, S11],[P, S12],[V13, S13],[V14, S14],[V15, S15]],
	[[V21, S21],[V22, S22],[Q, S23],[Q, S24],[V25, S25]], 2) :- cmp([P, S12], [Q, S24], [Q, S24])).
	
cmpWin(pair, pair, [[P, S11],[P, S12],[V13, S13],[V14, S14],[V15, S15]],
	[[V21, S21],[V22, S22],[V23, S23],[Q, S24],[Q, S25]], 1) :- cmp([P, S12], [Q, S25], [P, S12]).
cmpWin(pair, pair, [[P, S11],[P, S12],[V13, S13],[V14, S14],[V15, S15]],
	[[V21, S21],[V22, S22],[V23, S23],[Q, S24],[Q, S25]], 2) :- cmp([P, S12], [Q, S25], [Q, S25]).
	

	
cmpWin(pair, pair, [[V11, S11],[P, S12],[P, S13],[V14, S14],[V15, S15]],
	[[Q, S21],[Q, S22],[V23, S23],[V24, S24],[V25, S25]], 1) :- cmp([P, S13], [Q, S22], [P, S13]).
cmpWin(pair, pair, [[P, S11],[P, S12],[V12, S13],[V14, S14],[V15, S15]],
	[[Q, S21],[Q, S22],[V23, S23],[V24, S24],[V25, S25]], 2) :- cmp([P, S13], [Q, S22], [Q, S22]).
	
cmpWin(pair, pair, [[V11, S11],[P, S12],[P, S13],[V14, S14],[V15, S15]],
	[[V21, S21],[Q, S22],[Q, S23],[V24, S24],[V25, S25]], 1) :- cmp([P, S13], [Q, S23], [P, S13]).
cmpWin(pair, pair, [[V11, S11],[P, S12],[P, S13],[V14, S14],[V15, S15]],
	[[V21, S21],[Q, S22],[Q, S23],[V24, S24],[V25, S25]], 2) :- cmp([P, S13], [Q, S23], [Q, S23]).
	
cmpWin(pair, pair, [[V11, S11],[P, S12],[P, S13],[V14, S14],[V15, S15]],
	[[V21, S21],[V22, S22],[Q, S23],[Q, S24],[V25, S25]], 1) :- cmp([P, S13], [Q, S24], [P, S13]).
cmpWin(pair, pair, [[V11, S11],[P, S12],[P, S13],[V14, S14],[V15, S15]],
	[[V21, S21],[V22, S22],[Q, S23],[Q, S24],[V25, S25]], 2) :- cmp([P, S13], [Q, S24], [Q, S24]).
	
cmpWin(pair, pair, [[V11, S11],[P, S12],[P, S13],[V14, S14],[V15, S15]],
	[[V21, S21],[V22, S22],[V23, S23],[Q, S24],[Q, S25]], 1) :- cmp([P, S13], [Q, S25], [P, S13]).
cmpWin(pair, pair, [[V11, S11],[P, S12],[P, S13],[V14, S14],[V15, S15]],
	[[V21, S21],[V22, S22],[V23, S23],[Q, S24],[Q, S25]], 2) :- cmp([P, S13], [Q, S25], [Q, S25]).
	
	
cmpWin(pair, pair, [[V11, S11],[V12, S12],[P, S13],[P, S14],[V15, S15]],
	[[Q, S21],[Q, S22],[V23, S23],[V24, S24],[V25, S25]], 1) :- cmp([P, S14], [Q, S22], [P, S14]).
cmpWin(pair, pair, [[V11, S11],[V12, S12],[P, S13],[P, S14],[V15, S15]],
	[[Q, S21],[Q, S22],[V23, S23],[V24, S24],[V25, S25]], 2) :- cmp([P, S14], [Q, S22], [Q, S22]).
	
cmpWin(pair, pair, [[V11, S11],[V12, S12],[P, S13],[P, S14],[V15, S15]],
	[[V21, S21],[Q, S22],[Q, S23],[V24, S24],[V25, S25]], 1) :- cmp([P, S14], [Q, S23], [P, S14]).
cmpWin(pair, pair, [[V11, S11],[V12, S12],[P, S13],[P, S14],[V15, S15]],
	[[V21, S21],[Q, S22],[Q, S23],[V24, S24],[V25, S25]], 2) :- cmp([P, S14], [Q, S23], [Q, S23]).
	
cmpWin(pair, pair, [[V11, S11],[V12, S12],[P, S13],[P, S14],[V15, S15]],
	[[V21, S21],[V22, S22],[Q, S23],[Q, S24],[V25, S25]], 1) :- cmp([P, S14], [Q, S24], [P, S14]).
cmpWin(pair, pair, [[V11, S11],[V12, S12],[P, S13],[P, S14],[V15, S15]],
	[[V21, S21],[V22, S22],[Q, S23],[Q, S24],[V25, S25]], 2) :- cmp([P, S14], [Q, S24], [Q, S24]).
	
cmpWin(pair, pair, [[V11, S11],[V12, S12],[P, S13],[P, S14],[V15, S15]],
	[[V21, S21],[V22, S22],[V23, S23],[Q, S24],[Q, S25]], 1) :- cmp([P, S14], [Q, S25], [P, S14]).
cmpWin(pair, pair, [[V11, S11],[V12, S12],[P, S13],[P, S14],[V15, S15]],
	[[V21, S21],[V22, S22],[V23, S23],[Q, S24],[Q, S25]], 2) :- cmp([P, S14], [Q, S25], [Q, S25]).	
	
cmpWin(pair, pair, [[V11, S11],[V12, S12],[V12, S13],[P, S14],[P, S15]],
	[[Q, S21],[Q, S22],[V23, S23],[V24, S24],[V25, S25]], 1) :- cmp([P, S15], [Q, S22], [P, S15]).
cmpWin(pair, pair, [[V11, S11],[V12, S12],[V12, S13],[P, S14],[P, S15]],
	[[Q, S21],[Q, S22],[V23, S23],[V24, S24],[V25, S25]], 2) :- cmp([P, S15], [Q, S22], [Q, S22]).
	
cmpWin(pair, pair, [[V11, S11],[V12, S12],[V12, S13],[P, S14],[P, S15]],
	[[V21, S21],[Q, S22],[Q, S23],[V24, S24],[V25, S25]], 1) :-  cmp([P, S15], [Q, S23], [P, S15]).
cmpWin(pair, pair, [[V11, S11],[V12, S12],[V12, S13],[P, S14],[P, S15]],
	[[V21, S21],[Q, S22],[Q, S23],[V24, S24],[V25, S25]], 2) :-  cmp([P, S15], [Q, S23], [Q, S23]).
	
cmpWin(pair, pair, [[V11, S11],[V12, S12],[V12, S13],[P, S14],[P, S15]],
	[[V21, S21],[V22, S22],[Q, S23],[Q, S24],[V25, S25]], 1) :-  cmp([P, S15], [Q, S24], [P, S15]).
cmpWin(pair, pair, [[V11, S11],[V12, S12],[V12, S13],[P, S14],[P, S15]],
	[[V21, S21],[V22, S22],[Q, S23],[Q, S24],[V25, S25]], 2) :-  cmp([P, S15], [Q, S24], [Q, S24]).
	
cmpWin(pair, pair, [[V11, S11],[V12, S12],[V12, S13],[P, S14],[P, S15]],
	[[V21, S21],[V22, S22],[V23, S23],[Q, S24],[Q, S25]], 1) :-  cmp([P, S15], [Q, S25], [P, S15]).
cmpWin(pair, pair, [[V11, S11],[V12, S12],[V12, S13],[P, S14],[P, S15]],
	[[V21, S21],[V22, S22],[V23, S23],[Q, S24],[Q, S25]], 2) :-  cmp([P, S15], [Q, S25], [Q, S25]).
	
	
cmpWin(high_card, high_card, [H1,H2,H3,H4,H5], [Q1,Q2,Q3,Q4,Q5], 1) :-  cmp(H5, Q5, H5).
cmpWin(high_card, high_card, [H1,H2,H3,H4,H5], [Q1,Q2,Q3,Q4,Q5], 2) :-  cmp(H5, Q5, Q5).
	
%  Hand determination
getCombination([[10,X],[jack,X],[queen,X],[king,X],[ace,X]], royal_flush).

getCombination([[A,X],[B,X],[C,X],[D,X],[E,X]], straight_flush) :-
  successor(E,D), successor(D,C), successor(C,B), successor(B,A).

getCombination([[C,_],[A,_],[A,_],[A,_],[B,_]], four_of_a_kind) :-
  C = A ; B = A.

getCombination([[A,_],[B,_],[C,_],[D,_],[E,_]], full_house) :-
  A = B, D = E, (C = D ; C = B).

getCombination([[_,X],[_,X],[_,X],[_,X],[_,X]], flush).

getCombination([[A,_],[B,_],[C,_],[D,_],[E,_]], straight) :-
  successor(E,D), successor(D,C), successor(C,B), successor(B,A).

getCombination([[A,_],[B,_],[C,_],[D,_],[E,_]], three_of_a_kind) :-
  (A = B, B = C); (B = C, C = D); (C = D, D = E).

getCombination([[A,_],[A,_],[B,_],[B,_],[_,_]], two_pair).
getCombination([[_,_],[A,_],[A,_],[B,_],[B,_]], two_pair).
getCombination([[A,_],[A,_],[_,_],[B,_],[B,_]], two_pair).

getCombination([[A,_],[B,_],[C,_],[D,_],[E,_]], pair) :-
  A = B; B = C; C = D; D = E.

getCombination(_,high_card).


merge(L, [], L).
merge([], L, L).
merge([HA|TA], [HB|TB], LH) :- cmp(HA, HB, HA), merge([HA|TA], TB, LT), LH = [HB|LT].
merge([HA|TA], [HB|TB], LH) :- cmp(HA, HB, HB), merge(TA, [HB|TB], LT), LH = [HA|LT].

split([], [], []).
split([H|T1], E, [H|T2]) :- split(T1, T2, E).

sortHand([],[]).
sortHand([X],[X]).
sortHand(L,SL) :-
    L = [_,_|_],
	split(L, L1, L2),
	sortHand(L1, SL1), 
	sortHand(L2, SL2),
	merge(SL1, SL2, SL).

cmp([V,S1], [V,S2], [V, S]) :- cmp(S1, S2, S).
cmp([V1,S], [V2,_], [V1,S]) :- aGreaterB(V1, V2).
cmp([V1,_], [V2,S], [V2,S]) :- aGreaterB(V2, V1).

cmp(X, X, X).
cmp(X, Y, X) :- aGreaterB(X, Y).
cmp(X, Y, Y) :- aGreaterB(Y, X).

successor(royal_flush, straight_flush).   
successor(straigh_flush, four_of_a_kind).
successor(four_of_a_kind, full_house).    
successor(full_house, flush).
successor(flush, straight).               
successor(straight, three_of_a_kind).
successor(three_of_a_kind, two_pair).     
successor(two_pair, pair).
successor(pair, high_card).
successor(ace, king).     
successor(king, queen).   
successor(queen, jack).
successor(jack, 10).      
successor(10, 9).         
successor(9, 8).
successor(8, 7).          
successor(7, 6).          
successor(6, 5).
successor(5, 4).          
successor(4, 3).          
successor(3, 2).
successor(clubs, spades).
successor(spades, hearts).
successor(hearts, diamonds).

aGreaterB(X, Y) :-
  successor(X, Z),
  (Y = Z; aGreaterB(Z, Y)).


