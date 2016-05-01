:- use_module(tabling).
:- use_module(testlib).

%%%%% EXAMPLE 0 %%%%%

expected_variants([a(_,_)]).
% Note: a(3,_) is an empty table, but it is there...
expected_answers_for_variant(a(_,_),[a(1,2),a(2,3)]).

% The answers we expect for example 1, returned as a list with entries of the form X-Y. The order does not matter.
a_expected_answers([1-2,2-3]).

autotest(a_compare_answers).
% TEST: Tests anwers of example 0.
a_compare_answers :-
  compare_real_expected_answers(a,2,a_expected_answers).

go :-
  once(a(_X,_Y)).

a(X,Y) :-
  start_tabling(a(X,Y),p_aux(X,Y)).

p_aux(X,Y) :- e(X,Y).

% Test facts
e(1,2).
e(2,3).
