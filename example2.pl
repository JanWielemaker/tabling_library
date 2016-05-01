:- ['tabling.pl','testlib.pl','table_print.pl'].
:- use_module(library(format)).

%%%%% EXAMPLE 2 %%%%%%
% Meerdere recursieve calls met zelfde callpattern:
% b(X,Y) :- b(X,Z), b(Q,Y).
% For two given facts e(1,2) and e(2,3), there are four (!) solutions to b(X,Y):
% 1,2
% 2,3
% 1,3
% 2,2

expected_variants([b(_,_)]).
expected_answers_for_variant(b(_,_),[b(1,2),b(2,3),b(1,3),b(2,2)]).

% The answers we expect for example 2, returned as a list with entries of the form X-Y. The order does not matter.
b_expected_answers([1-2,2-3,1-3,2-2]).

autotest(b_compare_answers).
% TEST: Tests answers of example 2.
b_compare_answers :-
  compare_real_expected_answers(b,2,b_expected_answers).

go :-
  once(b(_X,_Y)).

b(X,Y) :-
  start_tabling(b(X,Y),b_aux(X,Y)).

b_aux(X,Y) :-
  writeln('before'), b(X,_Z), writeln('between'), b(_Q,Y).
b_aux(X,Y) :- e(X,Y).

% Test facts
e(1,2).
e(2,3).
