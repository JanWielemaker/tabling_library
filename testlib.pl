:- ['format.pl'].
:- use_module(library(lists)).
:- ['utils.pl'].

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
% Requires format/2.
% E = list that has the expected number of elements
% A = list that has the actual number of elements
expect_same_size(E,A) :-
  length(E,Es), % same_length/2 in library(lists) does not allow to print a detailed error message.
  length(A,As),
  (Es == As ->
    writeln('Success: lists have same size')
  ;
    format('BUG: expected list to have ~d elements but it had ~d~n',[Es,As])
  ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Requires format/2.
% Requires library(lists).
% Both lists are turned into sets first!
% E = list having expected elements
% A = list having actual elements
expect_lists_equal_sets(E,A) :-
  remove_duplicates(E,Es), % SWI: list_to_set
  remove_duplicates(A,As),
  ((list_difference_eq(As,Es,[]), list_difference_eq(Es,As,[])) -> % SWI: subtract
      writeln('Success: lists represent equal sets.')
  ;
    format('BUG: lists do not represent equal sets. Expected list was ~w, actual list was ~w~n',[E,A])
  ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% G = Name of goal to obtain real answers. Should take A free variables.
% A = Arity of goal to obtain real answers.
% E = Name of goal to obtain list of expected answers. Should take one argument.
compare_real_expected_answers(G,A,E) :-
  call(E,E2),
  length(FreeVarsList,A),
  G2 =.. [G|FreeVarsList],
  list_to_tuple(FreeVarsList,FreeVarsTuple),
  findall(FreeVarsTuple,G2,R),
  expect_same_size(E2,R),
  expect_lists_equal_sets(E2,R). 
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
  expect_same_size(ExpectedForVariant,AA2),
  expect_lists_equal_sets(ExpectedForVariant,AA2).
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
