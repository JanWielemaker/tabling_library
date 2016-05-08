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

:- module(table_link_manager,
	  [ table_link_manager_initialize/0,
	    get_existing_tables/1,		% -Tables
	    p_existing_table/2,			% +Variant, -TableID
	    p_link_variant_identifier/2,	% +Variant, -TableID
	    num_tables/1			% -Count
	  ]).
:- use_module(trie).

% This file defines a call pattern trie.
%
% This data structure keeps the relation between a variant and the corresponding table identifier using a trie. The trick is to make a canonical representation of a given variant using the numbervars/3 predicate. The trie uses this canonical representation as key, and the table identifier as value.

% Uses the (private) global variable trie_table_link

% This predicate should be called exactly once.
% It throws an exception if it is called more than once.

%%	table_link_manager_initialize
%
%	Initializes the global  variables   `trie_table_link`.  Normally
%	called from table_datastructure_initialize/0.

table_link_manager_initialize :-
  trie_new(EmptyTrie),
  nb_linkval(trie_table_link,EmptyTrie).

% Succeeds if there is a table TableIdentifier in existance for the given call variant Variant.
p_existing_table(Variant,TableIdentifier) :-
  nb_getval(trie_table_link,Trie),
  trie_lookup(Trie,Variant,TableIdentifier).

% Important remark: we cannot use an out-of-the-box association list, because we need a lookup based on variant checking, which is not available for such lists. Converting the association list to a regular list => why would you use an association list in the first place...
p_link_variant_identifier(Variant,TableIdentifier) :-
  nb_getval(trie_table_link,Trie),
  trie_insert_succeed(Trie,Variant,TableIdentifier),
  nb_linkval(trie_table_link,Trie).

% Returns a list of existing table identifiers.
% Rather costly.
get_existing_tables(Ts) :-
  nb_getval(trie_table_link,Trie),
  findall(T, trie_gen(Trie,_K, T),Ts).

% A very unefficient way of implementing this predicate. But it is only used for unit testing, so it doesn't really matter.
% Also, it doesn't require any additional bookkeeping during the actual execution.
num_tables(N) :-
  get_existing_tables(Ts),
  length(Ts,N).
