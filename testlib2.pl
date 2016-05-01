:- ['format.pl'].
:- ['table_link_manager.pl'].
:- ['variantlib.pl'].

% Used for automatically comparing the results of the XSB and hProlog version of a benchmark.
% Requires the presence of variant_for_xsb_comparison/1 facts in the hProlog benchmark file.
print_answers_for_xsb_comparison :-
  variant_for_xsb_comparison(Variant),
  print_answers_for_variant(Variant,'HPROLOGsol '),
  fail. % Backtrack to other variants
print_answers_for_xsb_comparison :-
  true. % No more variants

% requires a predicate expected_variants(-List) in the example
test_expected_variants_present :-
  expected_variants(Xs),
  test_expected_variants_present_(Xs),
  % now all expected variants are present.
  % next, we check whether there aren't any more present.
  length(Xs,NumExpected),
  num_tables(NumActual),
  assert_equal(NumExpected,NumActual,'test_expected_variants_present'),
  % write a note to show that we did the test.
  format:format('test_expected_variants_present succeeded~n',[]).
  
% uses "private" predicate from table_datastructure.gpp or table_link_manager.gpp depending on the version.
test_expected_variants_present_([]).
test_expected_variants_present_([X|Xs]) :-
  ( p_existing_table(X,_), ! % CUT ALTERNATIVE
  ;
    format:format('test_expected_variants_present: expected table for variant ~w was not there~n',[X]),
    throw('test_expected_variants_present: an expected table was not there!')
  ),
  test_expected_variants_present_(Xs).

% test whether all expected tables have received proper cleanup, that is: having the form complete_table/3
% uses "private" predicate from table_datastructure.gpp or table_link_manager.gpp depending on the version.
test_tables_cleaned :-
  expected_variants(Xs),
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
  expected_variants(Xs),
  test_answers_expected_tables_(Xs).

test_answers_expected_tables_([]) :-
  format:format('test_answers_expected_tables: all done!~n',[]).
test_answers_expected_tables_([Variant|Rest]) :-
  ( test_answers_for_expected_variant(Variant), ! % CUT ALTERNATIVE
  ;
    format:format('test_answers_expected_tables: could not test answers for expected variant ~w~n',[Variant]),
    throw('test_answers_expected_tables: could not test answers for an expected variant')
  ),
  test_answers_expected_tables_(Rest).

% ATTENTION: works only for ground answers in the tables (which we currently enforce when adding answers as well). To be on the safe side, an exception will be thrown if one of the expected answers is nonground.
% Requires a predicate expected_answers_for_variant/2 in the example.
% Uses "private" predicate from table_datastructure.gpp or table_link_manager.gpp depending on the version.
test_answers_for_expected_variant(Variant) :-
  % We really want a variant check here, not unification...
  expected_answers_for_variant(SomeVariant,ExpectedAnswers),
  variant(Variant,SomeVariant),
  p_existing_table(Variant,TableIdentifier),
  test_answers_for_variant_(ExpectedAnswers,TableIdentifier),
  % Now check that there are not more answers than expected
  length(ExpectedAnswers,NumExpected),
  get_num_answers(TableIdentifier,NumActual),
  assert_equal(NumExpected,NumActual,'test_answers_for_expected_variant'),
  % If we get here, write a success message.
  format:format('test_answers_for_expected_variant succeeded for variant ~w~n',[Variant]).

% Slow, but only used for testing. We don't need to keep the number of answers at runtime, so we don't keep track of it (for performance).
get_num_answers(TableIdentifier,NumActual) :-
  findall(A,get_answer(TableIdentifier,A),L),
  length(L,NumActual).

test_answers_for_variant_([],_TableIdentifier).
test_answers_for_variant_([ExpectedAnswer|Rest],TableIdentifier) :-
  ( ground(ExpectedAnswer), ! % CUT ALTERNATIVE
  ;
    format:format('test_answers_for_variant got nonground expected answer ~w, which it cannot handle correctly~n',[ExpectedAnswer]),
    throw('test_answers_for_variant will only work properly with ground expected answers, but got a nonground one')
  ),
  % get_answer => uses unification, so this won't work properly for nonground answers.
  ( get_answer(TableIdentifier,ExpectedAnswer), ! % CUT ALTERNATIVE
  ;
    format:format('test_answers_for_variant: could not get expected answer ~w~n',[ExpectedAnswer]),
    throw('test_answers_for_variant: could not get expected answer')
  ),
  test_answers_for_variant_(Rest,TableIdentifier).

assert_equal(NumExpected,NumActual,ContextualInfo) :-
  ( NumExpected == NumActual, ! % CUT ALTERNATIVE
  ;
    format:format('assert_equal failed in context of ~w: expected ~w but was ~w~n',[ContextualInfo,NumExpected,NumActual]),
    throw('assert_equal failed')
  ).
