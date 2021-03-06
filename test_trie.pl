/*  Part of SWI-Prolog

    Author:        Benoit Desouter <Benoit.Desouter@UGent.be>
		   Jan Wielemaker (SWI-Prolog port)
    Copyright (c)  2016, Benoit Desouter
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

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


