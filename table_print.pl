:- use_module(library(format)).
:- ['utils.pl'].

% Routines for printing the table datastructure.
% To assist in debugging and for output.

print_existing_tables :-
  get_existing_tables(Ts),
  format('EXISTING TABLES~n',[]),
  format('===============~n',[]),
  (
    member(T,Ts),
    get_call_variant(T,V),
    format('~w~n',[V]),
    fail
  ;
    format('==~n',[])
  ).

print_answers_for_table(T,PrefixText) :-
  get_call_variant(T,V),
  tbd_table_status(T,S),
  format('ANSWERS FOR TABLE ~w~n',[V]),
  format('======================~n',[]),
  format('Status: ~w~n',[S]),
  (
    get_answer(T,A),
    write(PrefixText),
    format('~w~n',[A]),
    fail
  ;
    format('==~n',[])
  ).

print_answers_for_table(T) :-
  print_answers_for_table(T,'').

print_answers_for_variant(V,PrefixText) :-
  table_for_variant(V,T),
  print_answers_for_table(T,PrefixText).

print_answers_for_variant(V) :-
  print_answers_for_variant(V,'').

print_answers_for_all_tables :-
  foreach_table_with_print(print_answers_for_table).

% Print a continuation C in human readable form.
% At the moment: print the first two components.
print_readable_continuation(Suspension) :-
  format('Suspension: ~w\n',[Suspension]).

print_continuations_for_table(_T) :-
  throw('call to deprecated predicate print_continuations for table - use print_worklist from table_datastructure.pl').

print_continuations_for_variant(V) :-
  table_for_variant(V,T),
  print_continuations_for_table(T).

print_continuations_for_all_tables :-
  foreach_table_with_print(print_continuations_for_table).
