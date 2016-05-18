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

:- module(testlib,
	  [ compare_real_expected_answers/3	% :Name, +Arity, :E
	  ]).
:- use_module(library(lists)).
:- use_module(table_utils).
:- use_module(library(dialect/hprolog)).

:- meta_predicate
	compare_real_expected_answers(:,+,1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% E = list that has the expected number of elements
% A = list that has the actual number of elements
expect_same_size(E,A,Result) :-
  length(E,Es),
  length(A,As),
  (   Es == As
  ->  Result = true
  ;   print_message(error,
		    format('expected list to have ~d elements but it had ~d~n',
			   [Es,As])),
      Result = false
  ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Both lists are turned into sets first!
% E = list having expected elements
% A = list having actual elements
expect_lists_equal_sets(E,A,True) :-
  list_to_set(E,Es),
  list_to_set(A,As),
  (   (   list_difference_eq(As,Es,[]),
	  list_difference_eq(Es,As,[])
      )
  ->  True = true
  ;   print_message(error, format('lists do not represent equal sets. \c
				   Expected list was ~p, actual list was ~p',[E,A]))
  ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% G = Name of goal to obtain real answers. Should take A free variables.
% A = Arity of goal to obtain real answers.
% E = Name of goal to obtain list of expected answers. Should take one argument.
compare_real_expected_answers(M:G,A,E) :-
  call(E,E2),
  length(FreeVarsList,A),
  G2 =.. [G|FreeVarsList],
  list_to_tuple(FreeVarsList,FreeVarsTuple),
  findall(FreeVarsTuple,M:G2,R),
  expect_same_size(E2,R,Ok1),
  expect_lists_equal_sets(E2,R,Ok2),
  Ok1 == true, Ok2 == true.
