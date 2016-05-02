:- use_module(tabling).
:- use_module(testlib).

%%%%% EXAMPLE 1 %%%%%

expected_variants([a(2,_),a(3,_),a(_,_)]).
% Note: a(3,_) is an empty table, but it is there...
expected_answers_for_variant(a(_,_),[a(1,2),a(2,3),a(1,3)]).
expected_answers_for_variant(a(2,_),[a(2,3)]).
expected_answers_for_variant(a(3,_),[]).

% The answers we expect for example 1, returned as a list with entries of the form X-Y. The order does not matter.
a_expected_answers([1-2,2-3,1-3]).

autotest(a_compare_answers).
% TEST: Tests anwers of example 1.
a_compare_answers :-
  compare_real_expected_answers(a,2,a_expected_answers).

go :-
  once(a(_X,_Y)).

:- table a/2.
a(X,Y) :- writeln('before'), a(X,Z), writeln('between'), a(Z,Y).
a(X,Y) :- e(X,Y).

% Test facts
e(1,2).
e(2,3).
