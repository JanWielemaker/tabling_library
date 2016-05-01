:- ensure_loaded(['tabling.pl','testlib.pl','table_print.pl']).
:- ensure_loaded(['format.pl']).

%%% EXAMPLE 5 %%%%
% Variation on example 1 but with a cycle. This is an important example for tabling.

expected_variants([f(1,_),f(2,_),f(3,_),f(_,_)]).
expected_answers_for_variant(f(_,_),L) :-
  findall(f(X,Y),(between(1,3,X),between(1,3,Y)),L).
expected_answers_for_variant(f(1,_),L) :-
  findall(f(1,X),between(1,3,X),L).
expected_answers_for_variant(f(2,_),L) :-
  findall(f(2,X),between(1,3,X),L).
expected_answers_for_variant(f(3,_),L) :-
  findall(f(3,X),between(1,3,X),L).

% The answers we expect for example 5, returned as a list with entries of the form X-Y. The order does not matter.
% We expect nine answers.
f_expected_answers([1-1,1-2,1-3,2-1,2-2,2-3,3-1,3-2,3-3]).

autotest(f_compare_answers).
% TEST: Tests anwers of example 5.
f_compare_answers :-
  %format('OLD TEST~n',[]),
  %compare_real_expected_answers(f,2,f_expected_answers),
  format('NEW TEST~n',[]),
  % Fill the table
  findall(_,f(_,_),_),
  get_existing_tables(Ts),
  length(Ts,NumTables),
  format('Number of tables after finding all the answers is: ~w~n', [NumTables]),
  print_existing_tables,
  % Test the results
  f_expected_answers(ExpectedAnswers),
  compare_expected_all_variants(ExpectedAnswers),
  get_existing_tables(Ts2),
  length(Ts2,NumTables2),
  format('Number of tables after testing the results is: ~w~n', [NumTables2]),
  print_answers_for_all_tables,
  get_existing_tables(Ts3),
  length(Ts3,NumTables3),
  format('Number of tables after printing the results for the second time is: ~w~n', [NumTables3]).

go :-
  once(f(_X,_Y)).

f(X,Y) :-
  start_tabling(f(X,Y),f_aux(X,Y)).

f_aux(X,Y) :-
  writeln('before'), f(X,Z), writeln('between'), f(Z,Y).
f_aux(X,Y) :- e2(X,Y).

% Test facts for example 5
e2(1,2).
e2(2,3).
e2(3,1).
