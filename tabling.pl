:- module(tabling,
	  [ start_tabling/2,		% +Wrapper, :Worker.

	    abolish_all_tables/0,

	    (table)/1,			% +PI ...
	    op(1150, fx, table)
	  ]).
:- use_module(double_linked_list).
:- use_module(table_datastructure).
:- use_module('batched-worklist').
:- use_module(wrapper).
:- use_module(testlib2).
:- use_module(globalWorklist).
:- use_module(library(lists)).

:- meta_predicate
	start_tabling(+, 0).

%%	user:exception(+Exception, +Var, -Action)
%
%	Realises lazy initialization of table variables.

user:exception(undefined_global_variable, Var, retry) :-
  (   table_gvar(Var)
  ->  true
  ;   format('Creating global var ~q~n', [Var]),
      nb_setval(Var, [])
  ).

table_gvar(trie_table_link) :-
  table_datastructure_initialize.
table_gvar(newly_created_table_identifiers) :-
  table_datastructure_initialize.
table_gvar(globalWorklist) :-
  nb_setval(globalWorklist, []).
table_gvar(leader) :-
  nb_setval(leader, []).

%%	abolish_all_tables
%
%	Remove all tables.  Should not be called when tabling is in
%	progress.
%
%	@bug	Check whether tabling is in progress

abolish_all_tables :-
  nb_delete(trie_table_link),
  nb_delete(newly_created_table_identifiers),
  nb_delete(globalWorklist),
  nb_delete(leader).


% Find table and status for the given call variant.
%
table_and_status_for_variant(V,T,S) :-
  % Order of the two calls really important: first create, then get status
  table_for_variant(V,T),
  tbd_table_status(T,S).

start_tabling(Wrapper,Worker) :-
  table_and_status_for_variant(Wrapper,T,S),
  ( S == complete ->
    get_answer(T,Wrapper)
  ;
    ( exists_scheduling_component ->
      run_leader(Wrapper,Worker,T),
      % Now answer the original query!
      get_answer(T,Wrapper)
    ;
      run_follower(S,Wrapper,Worker,T)
    )
  ).

run_follower(fresh,Wrapper,Worker,T) :-
  activate(Wrapper,Worker,T),
  shift(call_info(Wrapper,T)).

run_follower(active,Wrapper,_Worker,T) :-
  shift(call_info(Wrapper,T)).

run_leader(Wrapper,Worker,T) :-
  create_scheduling_component,
  activate(Wrapper,Worker,T),
  completion,
  unset_scheduling_component.

exists_scheduling_component :-
  nb_getval(leader,Leader),
  Leader == [].

create_scheduling_component :-
  nb_setval(leader,leaderCreated).

unset_scheduling_component :-
  nb_setval(leader,[]).

set_all_complete :-
  get_newly_created_table_identifiers(Ts,_NumIdentifiers),
  set_all_complete_(Ts).

set_all_complete_([]).
set_all_complete_([T|Ts]) :-
  set_complete_status(T),
  set_all_complete_(Ts).

cleanup_all_complete :-
  get_newly_created_table_identifiers(Ts,_NumIdentifiers),
  cleanup_all_complete_(Ts).

cleanup_all_complete_([]).
cleanup_all_complete_([T|Ts]) :-
  cleanup_after_complete(T),
  cleanup_all_complete_(Ts).

activate(Wrapper,Worker,T) :-
  set_active_status(T),
  (
    delim(Wrapper,Worker,T),
    fail
  ;
    true
  ).

delim(Wrapper,Worker,Table) :-
   reset(Worker,Continuation,SourceCall),
   ( Continuation == 0 ->
     add_answer(Table,Wrapper)
   ;
     SourceCall = call_info(_,SourceTable),
     TargetCall = call_info(Wrapper,Table),
     Dependency = dependency(SourceCall,Continuation,TargetCall),
     store_dependency(SourceTable,Dependency)
   ).

completion :-
  ( worklist_empty ->
    set_all_complete,
    cleanup_all_complete,
    % The place of the call to reset is really important: it must happen after the completion. If you do it before, you will wrongly remove yourself from the list of newly created table identifiers. On starting hProlog there are no newly created table identifiers, and nb_getval gives [] which is the perfect value.
    reset_newly_created_table_identifiers
  ;
    pop_worklist(Table),
    completion_step(Table),
    completion
  ).

completion_step(SourceTable) :-
  (
    table_get_work(SourceTable,Answer,dependency(Source,Continuation,Target)),
    Source = call_info(Answer,_),
    Target = call_info(Wrapper,TargetTable),
    delim(Wrapper,Continuation,TargetTable),
    fail
  ;
    true
  ).

table_get_work(Table,Answer,Dependency) :-
  get_worklist(Table,Worklist),
  % NOT IN PAPER (could be part of the definition of pop_worklist):
  unset_global_worklist_presence_flag(Worklist),
  set_flag_executing_all_work(Worklist),
  table_get_work_(Worklist,Answer,Dependency).

table_get_work_(Worklist,Answer,Dependency) :-
  worklist_do_all_work(Worklist,Answer,Dependency0), % This will eventually fail
  duplicate_term(Dependency0,Dependency).
table_get_work_(Worklist,_Answer,_Dependency) :-
  unset_flag_executing_all_work(Worklist),
  fail.

worklist_do_all_work(Worklist,Answer,Dependency) :-
  ( wkl_worklist_work_done(Worklist) ->
    fail
  ;
    worklist_do_step(Worklist,Answer,Dependency)
  ;
    worklist_do_all_work(Worklist,Answer,Dependency)
  ).

 worklist_do_step(Worklist,Answer,Dependency) :-
  wkl_p_get_rightmost_inner_answer_cluster_pointer(Worklist,ACP),
  wkl_p_swap_answer_continuation(Worklist,ACP,SCP),
  dll_get_data(ACP,wkl_answer_cluster(AList)),
  dll_get_data(SCP,wkl_suspension_cluster(SList)),
  member(Answer,AList),
  member(Dependency,SList).
