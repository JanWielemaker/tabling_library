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
	  [ expect_fail/1,			% +Goal
	    expect_true/1,			% +Goal
	    compare_real_expected_answers/3,	% :Name, +Arity, :E
	    compare_expected_all_variants/1	% +AllExpectedAnswers
	  ]).
:- use_module(library(lists)).
:- use_module(table_utils).
:- use_module(library(dialect/hprolog)).

:- meta_predicate
	expect_fail(0),
	expect_true(0),
	compare_real_expected_answers(:,+,1).

% Notes about testing:
% - name of a test should start with t_, optionally followed by a number
% (starting from 1), and followed by the name of the predicate it tests.
% - TODO: exceptions to this rule exist.
% - fully automated tests are marked with autotest(<<test_name>>).
% - for now, nothing is done with the autotest facts.

% Helper predicates for testing:

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Requires format/2.
expect_fail(G) :-
  (call(G) ->
    format('BUG: expected failure for goal ~w but it succeeded!~n',[G])
  ;
    writeln('Success: failure expected.')
  ).

% Requires format/2.
expect_true(G) :-
  (call(G) ->
    writeln('Success: true expected.')
  ;
    format('BUG: expected success for goal ~w but it failed!~n',[G])
  ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
  list_to_set(E,Es), % SWI: list_to_set
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Potential replacement for compare_real_expected_answers/3?
% TODO: finish and test it on nonbinary predicates. Update tests once it works.
% Potential pitfall: AllExpectedAnswers is not a callable predicate but a list. It is easy to add a small variation.
compare_expected_all_variants(AllExpectedAnswers) :-
  % order of arguments of compare_expected_for_variant/2 is unfortunate but logical therefore use flip/3 partially applied.
  %foreach_table(flip(compare_expected_for_variant,AllExpectedAnswers)).
  % Print some information identifying the table
  foreach_table(compare_expected_all_variants_(AllExpectedAnswers)).
  % TODO: now check whether you have had all tables you expected to be there.

compare_expected_all_variants_(AllExpectedAnswers,Table) :-
  get_call_variant(Table,Variant),
  format('Testing table ~w~n',[Variant]),
  format('=============================~n',[]),
  compare_expected_for_variant(Variant,AllExpectedAnswers).

% TODO: test and make sure it also works in nonbinary cases! Seems to work allright in binary cases.
% [foreach_table from utils.pl will help apply this on all call_variants!]
% Compare expected answers for a particular call variant
% IMPORTANT: at this point tables must be filled already, this is not done in this predicate!
% Uses expected_starting_with/3 to select the expected answers for that variant from all expected answers.
% V  = The particular call variant
% AE = All expected answers no matter the call variant
compare_expected_for_variant(V,AE) :-
  answers_for_variant(V,AA1), % AA1 = actual answers, format: [f(1,2)]
  format('answers_for_variant ~w: ~w~n',[V,AA1]),
  V  =.. [_F,StartWith|_Rest], % Won't work for nonbinary predicates.
  format('startWith = ~w, all (expected) answers = ~w~n',[StartWith,AE]),
  expected_starting_with(StartWith,AE,ExpectedForVariant), % format: list of tuples...
  format('expected_starting_with: ~w~n',[ExpectedForVariant]),
  predicate_list_to_tuple_list(AA1,AA2),
  format('tuple_list: ~w~n',[AA2]),
  expect_same_size(ExpectedForVariant,AA2,Ok1),
  expect_lists_equal_sets(ExpectedForVariant,AA2, Ok2),
  Ok1 == true, Ok2 == true.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% OK
test_expected_starting_with :-
  expected_starting_with(1,[1-2,2-1,1-3,3-1],E),
  writeln(E).

% TODO: works for binary predicates for which you specify the first argument. Generalize for a more general case!
% 1) First element of tuple
% 2) All answers
% 3) Answers starting with first element of tuple
expected_starting_with(S,A,E) :-
  expected_starting_with_(A,S,E-[]).

expected_starting_with_([],_A,E-E).
expected_starting_with_([X-_C2|Xs],A,H-T) :-
  X \= A, !, % cut should not be earlier
  expected_starting_with_(Xs,A,H-T).
expected_starting_with_([A-C2|Xs],A,H-T) :-
  expected_starting_with_(Xs,A,H-T2),
  T2 = [A-C2|T].
