:- ['tabling.pl','testlib.pl','table_print.pl'].
:- use_module(library(format)).

:- writeln('Reminder: call i1_compare_answers, ..., i5_compare_answers').

%%% EXAMPLE 7 %%%
% A more complicated graph example:
%
%       i1
%       |
%    +--+---+
%    |      |
%    v      v
%    i2     i3
%    |
% +--+---+
% |      |
% v      v
% i4 --> i5
%
% i1(X) :- i2(Y), X is Y + 1.
% i1(X) :- i3(Y), X is Y + 1.
%
% i2(X) :- i4(Y), X is Y + 1.
% i2(X) :- i5(Y), X is Y + 4. % !! + 4
%
% i4(X) :- i5(Y), X is Y + 1. % !! TABLE table7d
%
% i3(X) :- between(0,1,X). % !! TABLE table7c
%
% i5(Y) :- between(2,3,X).

expected_variants([i1(_),i2(_),i3(_),i4(_),i5(_)]).
expected_answers_for_variant(i1(_),[i1(5),i1(6),i1(7),i1(8),i1(1),i1(2)]).
expected_answers_for_variant(i2(_),[i2(4),i2(5),i2(6),i2(7)]).
expected_answers_for_variant(i3(_),[i3(0),i3(1)]).
expected_answers_for_variant(i4(_),[i4(3),i4(4)]).
expected_answers_for_variant(i5(_),[i5(2),i5(3)]).

autotest(i1_compare_answers).
i1_compare_answers :-
  compare_real_expected_answers(i1,1,i1_expected_answers).

autotest(i2_compare_answers).
i2_compare_answers :-
  compare_real_expected_answers(i2,1,i2_expected_answers).

autotest(i3_compare_answers).
i3_compare_answers :-
  compare_real_expected_answers(i3,1,i3_expected_answers).

autotest(i4_compare_answers).
i4_compare_answers :-
  compare_real_expected_answers(i4,1,i4_expected_answers).

autotest(i5_compare_answers).
i5_compare_answers :-
  compare_real_expected_answers(i5,1,i5_expected_answers).

i1_expected_answers([5,6,7,8,1,2]).
i2_expected_answers([4,5,6,7]).
i3_expected_answers([0,1]).
i4_expected_answers([3,4]).
i5_expected_answers([2,3]).

go :-
  once(i1(_)).

i1(X) :-
  start_tabling(i1(X),i1_aux(X)).

i2(X) :-
  start_tabling(i2(X),i2_aux(X)).

i3(X) :-
  start_tabling(i3(X),i3_aux(X)).

i4(X) :-
  start_tabling(i4(X),i4_aux(X)).

i5(X) :-
  start_tabling(i5(X),i5_aux(X)).

i1_aux(X) :- writeln('i1_aux 1: before'), i2(Y), writeln('i1_aux 1: after'), X is Y + 1.
i1_aux(X) :- writeln('i1_aux 2: before'), i3(Y), writeln('i1_aux 2: after'), X is Y + 1.
i2_aux(X) :- writeln('i2_aux 1: before'), i4(Y), writeln('i2_aux 1: after'), X is Y + 1.
i2_aux(X) :- writeln('i2_aux 2: before'), i5(Y), writeln('i2_aux 2: after'), X is Y + 4.
i4_aux(X) :- writeln('i4_aux 1: before'), i5(Y), writeln('i4_aux 1: after'), X is Y + 1.
i3_aux(X) :- between(0,1,X).
i5_aux(X) :- between(2,3,X).
