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

:- module(table_print,
	  [ print_existing_tables/0,
	    print_answers_for_all_tables/0,
	    print_answers_for_table/1,		% +Table
	    print_answers_for_table/2,		% +Table, +Prefix
	    print_answers_for_variant/1,	% +Variant
	    print_answers_for_variant/2		% +Variant, +Prefix
	  ]).
:- use_module(tabling).
:- use_module(table_utils).

% Routines for printing the table datastructure.
% To assist in debugging and for output.

print_existing_tables :-
	format('EXISTING TABLES~n',[]),
	format('===============~n',[]),
	forall(current_table(Variant, Trie),
	       format('~q: ~t~10|~p~n',[Trie, Variant])),
	format('==~n',[]).

print_answers_for_table(Trie, PrefixText) :-
	current_table(Variant, Trie),
	'$tbl_table_status'(Trie, Status),
	format('ANSWERS FOR TABLE ~q (~p)~n',[Trie, Variant]),
	format('================================~n',[]),
	format('Status: ~w~n',[Status]),
	forall(trie_gen(Trie, A, _),
	       format('~w~p~n', [PrefixText, A])),
	format('==~n',[]).

print_answers_for_table(Trie) :-
	print_answers_for_table(Trie,'').

print_answers_for_variant(Variant,PrefixText) :-
	current_table(Variant, Trie),
	print_answers_for_table(Trie, PrefixText).

print_answers_for_variant(V) :-
	print_answers_for_variant(V,'').

print_answers_for_all_tables :-
	foreach_table_with_print(print_answers_for_table).
