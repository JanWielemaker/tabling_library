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

:- module(tabling,
	  [ start_tabling/2,		% +Wrapper, :Worker.

	    current_table/2,		% ?Variant, ?Table

	    abolish_all_tables/0,

	    (table)/1,			% +PI ...
	    op(1150, fx, table)
	  ]).
:- use_module(wrapper).
:- use_module(library(lists)).
:- use_module(library(debug)).

:- meta_predicate
	start_tabling(+, 0).

%%	abolish_all_tables
%
%	Remove all tables.  Should not be called when tabling is in
%	progress.
%
%	@bug	Check whether tabling is in progress

abolish_all_tables :-
	'$tbl_abolish_all_tables'.


start_tabling(Wrapper,Worker) :-
	'$tbl_variant_table'(Wrapper, Trie, Status),
	(   Status == complete
	->  trie_gen(Trie, Wrapper, _)
	;   (   '$tbl_scheduling_component'(false, true)
	    ->  catch(run_leader(Wrapper, Worker, Trie), E, true),
	        (   var(E)
		->  trie_gen(Trie, Wrapper, _)
		;   '$tbl_table_discard_all',
		    throw(E)
		)
	    ;   run_follower(Status, Wrapper, Worker, Trie)
	    )
	).

run_follower(fresh, Wrapper, Worker, Trie) :- !,
	activate(Wrapper, Worker, Trie, Worklist),
	shift(call_info(Wrapper, Worklist)).
run_follower(Worklist, Wrapper, _Worker, _Trie) :-
	shift(call_info(Wrapper, Worklist)).

run_leader(Wrapper, Worker, Trie) :-
	activate(Wrapper, Worker, Trie, _Worklist),
	completion,
	'$tbl_scheduling_component'(_, false).

activate(Wrapper, Worker, Trie, WorkList) :-
	'$tbl_new_worklist'(WorkList, Trie),
	(   delim(Wrapper, Worker, WorkList),
	    fail
	;   true
	).

delim(Wrapper, Worker, WorkList) :-
	reset(Worker,SourceCall,Continuation),
	(   Continuation == 0
	->  '$tbl_wkl_add_answer'(WorkList, Wrapper)
	;   SourceCall = call_info(SrcWrapper, SourceWL),
	    TargetCall = call_info(Wrapper,    WorkList),
	    Dependency = dependency(SrcWrapper,Continuation,TargetCall),
	    '$tbl_wkl_add_suspension'(SourceWL, Dependency)
	).

completion :-
	'$tbl_pop_worklist'(WorkList), !,
	completion_step(WorkList),
	completion.
completion :-
	'$tbl_table_complete_all'.

completion_step(SourceTable) :-
	(   '$tbl_wkl_work'(SourceTable, Answer, Dependency),
	    dep(Answer, Dependency, Wrapper,Continuation,TargetTable),
	    delim(Wrapper,Continuation,TargetTable),
	    fail
	;   true
	).

dep(Answer, dependency(Answer, Continuation, call_info(Wrapper, TargetTable)),
    Wrapper, Continuation,TargetTable).



		 /*******************************
		 *	   EXAMINE DATA		*
		 *******************************/

current_table(Variant, Trie) :-
	'$tbl_variant_table'(VariantTrie),
	(   var(Variant)
	->  trie_gen(VariantTrie, Variant, Trie)
	;   trie_lookup(VariantTrie, Variant, Trie)
	).
