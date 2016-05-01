:- ensure_loaded(['tabling.pl','testlib.pl','table_print.pl']).
:- ensure_loaded(['format.pl']).

%%% EXAMPLE6 %%%
% Nested tabling: a tabled predicate calls another tabled predicate, but not mutually recursive.
% g(X) <- h(Y), X is Y + 1.
%
% h(X) <- h(Y), X is Y + 1, X < 5.
% h(0).

expected_variants([g(_),h(_)]).
expected_answers_for_variant(g(_),L) :-
  findall(g(X),between(1,5,X),L).
expected_answers_for_variant(h(_),L) :-
  findall(h(X),between(0,4,X),L).

g_expected_answers([1,2,3,4,5]).
h_expected_answers([0,1,2,3,4]).

autotest(g_compare_answers).
autotest(h_compare_answers).

g_compare_answers :-
  compare_real_expected_answers(g,1,g_expected_answers).

h_compare_answers :-
  compare_real_expected_answers(h,1,h_expected_answers).

go :-
  once(g(_)).

g(X) :-
  start_tabling(g(X),g_aux(X)).

h(X) :-
  start_tabling(h(X),h_aux(X)).

g_aux(X) :- writeln('g_aux: before calling h(Y)'), h(Y), writeln('g_aux: after calling h(Y)'), X is Y + 1.

h_aux(X) :- writeln('h_aux: before calling h(Y)'), h(Y), writeln('h_aux: after calling h(Y)'), X is Y + 1, X < 5.
h_aux(0).
