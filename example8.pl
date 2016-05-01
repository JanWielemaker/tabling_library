:- ensure_loaded(['tabling.pl','testlib.pl','table_print.pl']).
:- ensure_loaded(['format.pl']).

:- writeln('Reminder: call j_compare_answers').

%%% EXAMPLE 8 %%%
% Example specifically designed to test whether we can get a new continuation in the last iteration of run_contins.
% This is indeed the case. Thus in run_contins we also need to look at the number of continuations to determine whether we can stop.
% However we had some questions: we might need to look whether the new continuation is a variant of a continuation that is already present (to avoid infinite loops).
%
% j(a,b).
% j(X,Y) :- j(_,_), j(Y,X).

expected_variants([j(_,_)]).
expected_answers_for_variant(j(_,_),[j(a,b),j(b,a)]).

autotest(j_compare_answers).
j_compare_answers :-
  compare_real_expected_answers(j,2,j_expected_answers).

j_expected_answers([a-b,b-a]).

go :-
  once(j(_X,_Y)).

j_aux(a,b) :- writeln('j_aux 1: give answer'). % TODO: check why we don*t have to save this answer here explicitly.
j_aux(X,Y) :- writeln('j_aux 2: before'), j(_,_), writeln('j_aux 2: between'), j(Y,X), writeln('j_aux 2: after').

j(X,Y) :-
  start_tabling(j(X,Y),j_aux(X,Y)).
