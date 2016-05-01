:- ['tabling.pl','testlib.pl','table_print.pl'].
:- ['format.pl'].

%%%%% EXAMPLE 3: shuttle %%%%%
% Answers: all integers between -10 and 10 (included).
%
% Consider the execution of the query {\tt ?- p$_g$(Y).} which
% succeeds for all integers between {\tt -10} and {\tt 10} (included)
% against the following tabled program:
% :- table p/1. \\
% p(X) :- p$_{c_1}$(Y), 0 =< Y, Y < 10, X is -Y - 1.  \\
% p(X) :- p$_{c_2}$(Y), -10 < Y, Y =< 0, X is -Y + 1. \\
% p(0).
% The two consumers that are encountered have been given an index for
% ease of reference.  The abstract machine needs to alternate between
% consumers {\tt p$_{c_1}$(Y)} and {\tt p$_{c_2}$(Y)} multiple times
% before all answers have been generated.

expected_variants([c(_)]).
expected_answers_for_variant(c(_),L) :-
  findall(c(X),between(-10,10,X),L).

c_expected_answers(L) :-
  findall(X,between(-10,10,X),L).

autotest(c_compare_answers).
% TEST: Tests answers of example 3.
c_compare_answers :-
  compare_real_expected_answers(c,1,c_expected_answers).

go :-
  once(c(_)).

c(X) :-
  start_tabling(c(X),c_aux(X)).

c_aux(X) :- writeln('before 1'), c(Y), format:format('after recursive 1: Y is ~w, and X is ~w~n',[Y,X]), 0 =< Y, Y < 10, X is -Y-1, writeln('at end of recursive 1').
c_aux(X) :- writeln('before 2'), c(Y), format:format('after recursive 2: Y is ~w, and X is ~w~n',[Y,X]), -10 < Y, Y =< 0, X is -Y+1, writeln('at end of recursive 2').
c_aux(0).
