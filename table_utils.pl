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

:- module(table_utils,
	  [ print_trail_size/1,			% +Message
	    foreach_table/1,			% +Goal
	    foreach_table_with_print/1,		% +Goal
	    list_to_tuple/2,			% +List, -Tuple
	    predicate_to_tuple/2,		% +Term, -Tuple
	    predicate_list_to_tuple_list/2	% +ListOfTerms, -ListOfTuples
	  ]).
:- use_module(tabling).
:- use_module(library(apply)).
:- meta_predicate
	foreach_table(1),
	foreach_table_with_print(1).

% Predicates of a general nature that may be useful in a lot of places.

print_trail_size(Message) :-
  write(Message), write(' '),
  print_trail_size.

% Debugging the trail stack
print_trail_size :-
  trail_size(S),
  format('Trail size is now ~w~n',[S]).

:- if(current_predicate(sysh:internalstat/5)).
trail_size(S) :-
  sysh:internalstat(3,_,S,_,x).
:- else.
trail_size(S) :-
  statistics(trailused, S).
:- endif.

% Perform goal for all existing tables, first printing
% >FOR EACH TABLE
% >END FOR EACH TABLE
% Goal should take 1 parameter: the name of the table.
foreach_table_with_print(Goal) :-
  % Prefixing with module name ('format:') is a hack. I suspect hProlog does not implement modules and meta_predicate completely correct.
  foreach_table(Goal,
		format('>FOR EACH TABLE\n',[]),
		format('>END FOR EACH TABLE\n', [])).

% Perform goal for all existing tables
% Goal should take 1 parameter: the name of the table.
foreach_table(Goal) :-
  foreach_table(Goal,true,true).

% Perform goal for all existing tables
% Goal should take 1 parameter: the name of the table.
% PreForeachGoal and PostForeachGoal should not take any more parameters.
% SWI-Prolog :- meta_predicate foreach_table(1,0,0).
foreach_table(Goal,PreForeachGoal,PostForeachGoal) :-
  call(PreForeachGoal),
  forall(current_table(Variant, Trie),
	 call(Goal, table(Variant, Trie))),
  call(PostForeachGoal).

% Convert [f(X,Y,...), ...] to [X-Y-...,...]
% There is no check on the function symbol
predicate_list_to_tuple_list(PL,TL) :-
  maplist(predicate_to_tuple,PL,TL).

% f(X,Y) -> X-Y
% Does not work on facts.
predicate_to_tuple(P,T) :-
  P =.. [_Functor|Args],
  list_to_tuple(Args,T).

% [X,Y,Z] -> X-Y-Z
% Empty list. No sensible behaviour. Throw exception.
list_to_tuple([],_) :-
  throw('list_to_tuple called on empty list').
% List with at least one element.
list_to_tuple([First|Rest],Tuple) :-
  foldl(to_tuple,Rest,First,Tuple).

% E = 'element'
% Ai = 'accumulator in'
to_tuple(E,Ai,Ai-E).
