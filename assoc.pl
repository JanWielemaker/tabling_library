%   Copyright(C) 1994, Swedish Institute of Computer Science

%   File       : ASSOC.PL
%   Maintainer : Mats Carlsson
%   Updated    : 15 December 1994
%   Purpose    : AVL tree implementation of "association lists".


%% :- meta_predicate
%% 	map_assoc(:, ?),
%% 	map_assoc(:, ?, ?).

%% :- use_module(library(lists), [
%% 	append/3
%% 			      ]).


%   Adapted from shared assoc.pl, which used binary trees,
%   written by Richard A O'Keefe.

%   In this package, finite functions are represented by AVL trees, i.e.
%   they are subject to the Adelson-Velskii-Landis balance criterion:
%   
%     A tree is balanced iff for every node the heights of its
%     two subtrees differ by at most 1.
%   
%   The empty tree is represented as t.
%   A tree with key K, value V, and left and right subtrees L and R is
%   represented as t(K,V,|R|-|L|,L,R).
%   |T| denotes the height of T.
%   
%   The advantage of this representation is that lookup, insertion and
%   deletion all become - in the worst case - O(log n) operations.
%   
%   The algorithms are due to Wirth, "Algorithms + Data Structures =
%   Programs", 4.4.6 - 4.4.8.

% adapted by Bart Demoen on 12 August 2002 to hProlog for HAL

%   empty_assoc(?Assoc)
%   is true when Assoc is an empty AVL tree.

empty_assoc(t).					% also in Compiler/comp_sup.pl



%   assoc_to_list(+Assoc, ?List)
%   assumes that Assoc is a proper AVL tree, and is true when
%   List is a list of Key-Value pairs in ascending order with no
%   duplicate keys specifying the same finite function as Assoc.
%   Use this to convert an Assoc to a list.

assoc_to_list(Assoc, List) :-
	assoc_to_list(Assoc, List, []).	% in Compiler/comp_sup.pl

assoc_to_list(t) --> [].
assoc_to_list(t(K,V,_,L,R)) -->
	assoc_to_list(L),
	[K-V],
	assoc_to_list(R).



%   gen_assoc(?Key, +Assoc, ?Value)
%   assumes that Assoc is a proper AVL tree, and is true when
%   Key is associated with Value in Assoc.  Can be used to enumerate
%   all Values by ascending Keys.

%% gen_assoc(Key, Assoc, Value) :-
%% 	prolog:gen_assoc(Key, Assoc, Value).	% in Compiler/comp_sup.pl


gen_assoc(Key, t(K,V,_,L,R), Val) :-
	(   gen_assoc(Key, L, Val)
	;   Key = K, Val = V
	;   gen_assoc(Key, R, Val)
	).



%   get_assoc(+Key, +Assoc, ?Value)
%   assumes that Assoc is a proper AVL tree.  It is true when
%   Key is identical to (==) one of the keys in Assoc, and Value
%   unifies with the associated value.

%% get_assoc(Key, Assoc, Value) :-
%% 	prolog:get_assoc(Key, Assoc, Value).	% in Compiler/comp_sup.pl


get_assoc(Key, t(K,V,_,L,R), Val) :-
	compare(Rel, Key, K),
	get_assoc(Rel, Key, Val, V, L, R).

get_assoc(<, Key, Val, _, Tree, _) :- get_assoc(Key, Tree, Val).
get_assoc(=, _, Val, Val, _, _).
get_assoc(>, Key, Val, _, _, Tree) :- get_assoc(Key, Tree, Val).



%   get_assoc(+Key, +OldAssoc, ?OldValue, ?NewAssoc, ?NewValue)
%   is true when OldAssoc and NewAssoc are AVL trees of the same
%   shape having the same elements except that the value for Key in
%   OldAssoc is OldValue and the value for Key in NewAssoc is NewValue.

%% get_assoc(Key, OldAssoc, OldValue, NewAssoc, NewValue) :-
%% 	prolog:get_assoc(Key, OldAssoc, OldValue, NewAssoc, NewValue).	% in Compiler/comp_sup.pl

get_assoc(Key, t(K0,V0,B,L0,R0), Val0, t(K,V,B,L,R), Val) :-
	compare(Rel, Key, K0),
	get_assoc(Rel, Key, K0, V0, L0, R0, Val0, K, V, L, R, Val).

get_assoc(<, Key, K, V, Tree0, R, Val0, K, V, Tree, R, Val) :-
	get_assoc(Key, Tree0, Val0, Tree, Val).
get_assoc(=, _, K, Val0, L, R, Val0, K, Val, L, R, Val).
get_assoc(>, Key, K, V, L, Tree0, Val0, K, V, L, Tree, Val) :-
	get_assoc(Key, Tree0, Val0, Tree, Val).


%   put_assoc(+Key, +OldAssoc, +Val, -NewAssoc)
%   is true when OldAssoc and NewAssoc define the same finite function
%   except that NewAssoc associates Val with Key.  OldAssoc need not have
%   associated any value at all with Key.  

put_assoc(Key, Assoc0, Val, Assoc1) :-
	put_assoc(Assoc0, Key, Val, Assoc1, _).	% in Compiler/comp_sup.pl


put_assoc(t,            Key, Val, t(Key,Val,0,t,t), 1).
put_assoc(t(K,V,B,L,R), Key, Val, Result, Delta) :-
	compare(O, Key, K),
	put_assoc(O, Key, Val, Result, Delta, K, V, B, L, R).


put_assoc(<, Key, Val, Assoc, Delta, K, V, B, L, R) :-
	put_assoc(L, Key, Val, Lassoc, D1),
        % Benoit: \(IntExpr) is bitwise negation and /\ is bitwise and.
	Delta is \(B) /\ D1,			% grew?
	B1 is B-D1,
	assoc(B1, K, V, Lassoc, R, Assoc).
put_assoc(=, Key, Val, t(Key,Val,B,L,R), 0, _, _, B, L, R).
put_assoc(>, Key, Val, Assoc, Delta, K, V, B, L, R) :-
	put_assoc(R, Key, Val, Rassoc, D1),
	Delta is \(B) /\ D1,			% grew?
	B1 is B+D1,
	assoc(B1, K, V, L, Rassoc, Assoc).



assoc(-2, K, V, L, R, Assoc) :-
	L = t(K1,V1,B1,L1,R1),
	assoc_left(B1, K1, V1, L1, R1, K, V, R, Assoc).
assoc(-1, K, V, L, R, t(K,V,-1,L,R)).
assoc( 0, K, V, L, R, t(K,V, 0,L,R)).
assoc( 1, K, V, L, R, t(K,V, 1,L,R)).
assoc( 2, K, V, L, R, Assoc) :-
	R = t(K1,V1,B1,L1,R1),
	assoc_right(B1, K1, V1, L1, R1, K, V, L, Assoc).

assoc_left(-1, K1, V1, L1, R1, K, V, R,		% single LL rotation
	    t(K1,V1, 0,L1,t(K,V, 0,R1,R))).
assoc_left( 0, K1, V1, L1, R1, K, V, R,		% single LL rotation
	    t(K1,V1, 1,L1,t(K,V,-1,R1,R))).
assoc_left( 1, K1, V1, L1, R1, K, V, R,		% double LR rotation
	    t(K2,V2, 0,t(K1,V1,BK1,L1,L2),t(K,V,BK,R2,R))) :-
        R1 = t(K2,V2,B2,L2,R2),
	assoc(B2, BK1, BK).

assoc_right( 1, K1, V1, L1, R1, K, V, L,	% single RR rotation
	     t(K1,V1, 0,t(K,V, 0,L,L1),R1)).
assoc_right( 0, K1, V1, L1, R1, K, V, L,	% single RR rotation
	     t(K1,V1,-1,t(K,V, 1,L,L1),R1)).
assoc_right(-1, K1, V1, L1, R1, K, V, L,	% double RL rotation
	     t(K2,V2, 0,t(K,V,BK,L,L2),t(K1,V1,BK1,R2,R1))) :-
        L1 = t(K2,V2,B2,L2,R2),
	assoc(B2, BK, BK1).

assoc(-1,  0, 1).
assoc( 0,  0, 0).
assoc( 1, -1, 0).


%   map_assoc(:Pred, ?Assoc)
%   is true when Assoc is an association tree, and for each Key, 
%   if Key is associated with Value in Assoc, Pred(Value) is true.

%% map_assoc(MPred, Assoc) :-
%% 	prolog:get_module(MPred, Pred, M),
%% 	map_assoc_1(Assoc, M, Pred).
%% 
%% map_assoc_1(t, _, _).
%% map_assoc_1(t(_,Val,_,L,R), M, Pred) :-
%% 	map_assoc_1(L, M, Pred),
%% 	add_arguments(Pred, [Val], Goal),
%% 	M:Goal,
%% 	map_assoc_1(R, M, Pred).

map_assoc(Pred,Assoc) :- map_assoc_1(Assoc,Pred).
map_assoc_1(t,_).
map_assoc_1(t(_,Val,_,L,R),Pred) :-
	call(Pred,Val),
	map_assoc_1(L,Pred),
	map_assoc_1(R,Pred).


%   map_assoc(:Pred, ?OldAssoc, ?NewAssoc)
%   is true when OldAssoc and NewAssoc are association trees of the
%   same shape, and for each Key, if Key is associated with Old in
%   OldAssoc and with New in NewAssoc, Pred(Old,New) is true.

map_assoc(Pred, OldAssoc, NewAssoc) :-
	map_assoc_1(OldAssoc, NewAssoc, Pred).

map_assoc_1(t, t, _).
map_assoc_1(t(Key,Old,B,L0,R0), t(Key,New,B,L1,R1), Pred) :-
	map_assoc_1(L0, L1, Pred),
	call(Pred,Old,New),
	map_assoc_1(R0, R1, Pred).

%% map_assoc(MPred, OldAssoc, NewAssoc) :-
%% 	prolog:get_module(MPred, Pred, M),
%% 	map_assoc_1(OldAssoc, NewAssoc, M, Pred).
%% 
%% map_assoc_1(t, t, _, _).
%% map_assoc_1(t(Key,Old,B,L0,R0), t(Key,New,B,L1,R1), M, Pred) :-
%% 	map_assoc_1(L0, L1, M, Pred),
%% 	add_arguments(Pred, [Old,New], Goal),
%% 	M:Goal,
%% 	map_assoc_1(R0, R1, M, Pred).

%% add_arguments(Goal, Args, Goal1) :-
%% 	Goal =.. GoalList,
%% 	append(GoalList, Args, GoalList1),
%% 	Goal1 =.. GoalList1.
