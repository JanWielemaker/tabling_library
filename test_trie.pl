:- module(test_trie,
	  [ test_trie/0
	  ]).
:- use_module(trie).
:- use_module(library(plunit)).
:- use_module(library(debug)).

test_trie :-
	run_tests([ trie
		  ]).

:- begin_tests(trie).

test(insert, T2 =@= T) :-
	T = a(1,2),
	trie_new(Trie),
	trie_insert(Trie, T, T),
	trie_lookup(Trie, T, T2).
test(insert_bt, T2 =@= T) :-
	T = a(1,2),
	trie_new(Trie),
	freeze_stack,
	(   trie_insert(Trie, T, T),
	    trie_lookup(Trie, T, T2),
	    assertion(T2 =@= T),
	    fail
	;   trie_lookup(Trie, T, T2),
	    assertion(T2 =@= T)
	).

freeze_stack :-
	numlist(1, 1000, L),
	nb_setval(x, L).

:- end_tests(trie).


