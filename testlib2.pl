:- module(testlib2,
	  [ test_expected_variants_present/0,
	    test_tables_cleaned/0,
	    test_answers_expected_tables/0,
	    test_answers_for_expected_variant/1
	  ]).
:- use_module(table_link_manager).
:- use_module(table_datastructure).
:- use_module(library(terms)).

:- module_transparent
	test_expected_variants_present/0,
	test_tables_cleaned/0,
	test_answers_expected_tables/0.
:- meta_predicate
	test_answers_for_expected_variant(:).

% requires a predicate expected_variants(-List) in the example
test_expected_variants_present :-
  context_module(M),
  test_expected_variants_present(M).

test_expected_variants_present(M) :-
  M:expected_variants(Xs0),
  maplist(mqualify(M), Xs0, Xs),
  test_expected_variants_present_(Xs, True),
  True \== false,
  % now all expected variants are present.
  % next, we check whether there aren't any more present.
  length(Xs,NumExpected),
  num_tables(NumActual),
  assert_equal(NumExpected,NumActual,'test_expected_variants_present').

mqualify(M,T,M:T).

% uses "private" predicate from table_datastructure.gpp or table_link_manager.gpp depending on the version.
test_expected_variants_present_([], _).
test_expected_variants_present_([X|Xs], True) :-
  (   p_existing_table(X,_)
  ->  true
  ;   print_message(error, format('Missing table for variant ~p',[X])),
      True = false
  ),
  test_expected_variants_present_(Xs, True).

% test whether all expected tables have received proper cleanup, that is: having the form complete_table/3
% uses "private" predicate from table_datastructure.gpp or table_link_manager.gpp depending on the version.
test_tables_cleaned :-
  context_module(M),
  M:expected_variants(Xs),
  test_tables_cleaned_(Xs),
  % if we get here, write a note to show that we did the test.
  format:format('test_tables_cleaned succeeded~n',[]).

test_tables_cleaned_([]).
test_tables_cleaned_([X|Xs]) :-
  ( p_existing_table(X,TableIdentifier),
    nb_getval(TableIdentifier,Table),
    functor(Table,complete_table,2), ! % CUT ALTERNATIVE
  ;
    format:format('test_tables_cleaned: table for variant ~w did not receive proper cleanup~n',[X]),
    throw('test_tables_cleaned: a table did not receive proper cleanup')
  ),
  test_tables_cleaned_(Xs).

test_answers_expected_tables :-
  context_module(M),
  test_answers_expected_tables(M).

test_answers_expected_tables(M) :-
  M:expected_variants(Xs0),
  maplist(mqualify(M), Xs0, Xs),
  test_answers_expected_tables_(Xs, True),
  True \== false.

test_answers_expected_tables_([], _).
test_answers_expected_tables_([Variant|Rest], True) :-
  (   test_answers_for_expected_variant(Variant)
  ->  true
  ;   print_message(error, format('Wrong answers for expected variant ~p',[Variant])),
      True = false
  ),
  test_answers_expected_tables_(Rest, True).

% ATTENTION: works only for ground answers in the tables (which we currently enforce when adding answers as well). To be on the safe side, an exception will be thrown if one of the expected answers is nonground.
% Requires a predicate expected_answers_for_variant/2 in the example.
% Uses "private" predicate from table_datastructure.gpp or table_link_manager.gpp depending on the version.
test_answers_for_expected_variant(M:Variant) :-
  % We really want a variant check here, not unification...
  M:expected_answers_for_variant(SomeVariant,ExpectedAnswers0),
  maplist(mqualify(M), ExpectedAnswers0, ExpectedAnswers),
  variant(Variant,SomeVariant),
  p_existing_table(M:Variant,TableIdentifier),
  test_answers_for_variant_(ExpectedAnswers,TableIdentifier, True),
  True \== false,
  % Now check that there are not more answers than expected
  length(ExpectedAnswers,NumExpected),
  get_num_answers(TableIdentifier,NumActual),
  assert_equal(NumExpected,NumActual,'test_answers_for_expected_variant').

% Slow, but only used for testing. We don't need to keep the number of answers at runtime,
% so we don't keep track of it (for performance).
get_num_answers(TableIdentifier,NumActual) :-
  findall(A,get_answer(TableIdentifier,A),L),
  length(L,NumActual).

test_answers_for_variant_([],_TableIdentifier, _).
test_answers_for_variant_([ExpectedAnswer|Rest],TableIdentifier, True) :-
  (   ground(ExpectedAnswer)
  ->  true
  ;   print_message(error, format('Got nonground expected answer ~p, \c
				   which it cannot handle correctly',[ExpectedAnswer])),
      True = false
  ),
  % get_answer => uses unification, so this won't work properly for nonground answers.
  (   get_answer(TableIdentifier,ExpectedAnswer)
  ->  true
  ;   print_message(error, format('Missing expected answer ~p',[ExpectedAnswer])),
      True = false
  ),
  test_answers_for_variant_(Rest,TableIdentifier,True).

assert_equal(NumExpected,NumActual,ContextualInfo) :-
  (   NumExpected == NumActual
  ->  true
  ;   print_message(error, format('assert_equal failed in context of ~w: \c
				   expected ~w but was ~w~n',
				  [ContextualInfo,NumExpected,NumActual])),
      fail
  ).
