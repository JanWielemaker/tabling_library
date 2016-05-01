:- ['tabling.pl','testlib.pl','table_print.pl'].
:- use_module(library(format)).

% The reachability predicate can be written in several variants.
% Example 1: a
% p(X,Y) :- p(X,Z), p(Z,Y).
% p(X,Y) :- e(X,Y).
%
% Example 9a:
% p(X,Y) :- p(X,Z), e(Z,Y).
% p(X,Y) :- e(X,Y).
%
% Example 9b:
% p(X,Y) :- e(X,Z), p(Z,Y).
% p(X,Y) :- e(X,Y).
%
% Example 9c:
% p(X,Y) :- e(X,Y).
% p(X,Y) :- p(X,Z), p(Z,Y).
%
% Example 9d:
% p(X,Y) :- e(X,Y).
% p(X,Y) :- p(X,Z), e(Z,Y).
%
% Example 9e:
% p(X,Y) :- e(X,Y).
% p(X,Y) :- e(X,Z), p(Z,Y).

expected_variants([a(_,_)]).
expected_answers_for_variant(a(_,_),[a(1,2),a(2,3),a(1,3)]).

% TODO: rewrite all these examples without using dumb copy-paste-modify

% The answers we expect for reachability variants, returned as a list with entries of the form X-Y. The order does not matter.
reachability_expected_answers([1-2,2-3,1-3]).
autotest(a_compare_answers).
a_compare_answers :-
  compare_real_expected_answers(a,2,reachability_expected_answers).

go :-
  once(a(_X,_Y)).

a(X,Y) :-
  start_tabling(a(X,Y),p_aux(X,Y)).

% 9a
p_aux(X,Y) :-
  writeln('before'), a(X,Z), format:format('between: prove ~w~n',[e(Z,Y)]),  e(Z,Y).
p_aux(X,Y) :- e(X,Y).

% Test facts for examples 1 and 2.
e(1,2).
e(2,3).
