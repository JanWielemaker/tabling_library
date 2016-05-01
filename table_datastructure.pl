:- module(table_datastructure,
	  [ table_datastructure_initialize/0,
	    get_answer/2,			% +TableID, -Answer
	    add_answer/2,			% +TableID, +Answer
	    get_call_variant/2,			% +TableID, -CallVariant
	    set_complete_status/1,		% +TableID
	    set_active_status/1,		% +TableID
	    tbd_table_status/2,			% +TableID, -Status
	    table_for_variant/2,		% +Variant, -TableID
	    get_worklist/2,			% +TableID, -WorkList
	    store_dependency/2,			% +TableID, +Suspension
	    cleanup_after_complete/1,		% +TableID
	    get_newly_created_table_identifiers/2, % NewlyCreatedTableIDs, NumIDs
	    reset_newly_created_table_identifiers/0,
	    answers_for_variant/2		% +Variant, -Answers
	  ]).
:- use_module(table_link_manager).
:- use_module(trie).
:- use_module('batched-worklist').
:- use_module(gensymlib).

% This file defines the table datastructure.
%
% The table datastructure contains the following sub-structures:
% - the answer trie
% - the worklist
%
% Structure for tables:
% table(CallVariant,Status,AnswerTrie,Worklist) or complete_table(CallVariant,AnswerTrie).
% where AnswerTrie contains a trie of unique answers
%
% Remember that a table may also be nonexistent!
% nb_getval(nonexistent,X) then gives [].

% Initialization!
% This predicate should be called exactly once.
% It throws an exception if it is called more than once.
table_datastructure_initialize :-
  ( table_datastructure_initialized ->
    throw('table_datastructure_initialize: already initialized - 2nd call not allowed')
  ;
    % Do initialization
    table_link_manager_initialize,
    reset_newly_created_table_identifiers % Put default value in global variable
  ).

table_datastructure_initialized :-
  table_link_manager_initialized.

% Returns a list of newly created table identifiers since the last call to reset_newly_created_table_identifiers/0, as well as the length of the list.
get_newly_created_table_identifiers(NewlyCreatedTableIdentifiers,NumIdentifiers) :-
  nb_getval(newly_created_table_identifiers,NewlyCreatedTableIdentifiers-NumIdentifiers).

reset_newly_created_table_identifiers :-
  nb_linkval(newly_created_table_identifiers,[]-0).

add_to_newly_created_table_identifiers(TableIdentifier) :-
  nb_getval(newly_created_table_identifiers,L1-Num1),
  Num2 is Num1 + 1,
  nb_linkval(newly_created_table_identifiers,[TableIdentifier|L1]-Num2).

% PRIVATE
% Mode: + -
%
% Created in the fresh status.
p_create_table(CallVariant,TableIdentifier) :-
  % We use a copy_term here so that we can be sure not to corrupt our table if CallVariant is "changed" afterwards.
  copy_term(CallVariant,CallVariant2),
  % Generate a table identifier, create the table and do bookkeeping.
  gensym(table,TableIdentifier),
  % Create a trie and a worklist.
  trie_new(EmptyTrie),
  wkl_new_worklist(TableIdentifier,NewWorklist),
  nb_linkval(TableIdentifier,table(CallVariant2,fresh,EmptyTrie,NewWorklist)),
  p_link_variant_identifier(CallVariant2,TableIdentifier),
  add_to_newly_created_table_identifiers(TableIdentifier).

% Get the Status for table TableIdentifier
% Throws exception if this table does not exist.
tbd_table_status(TableIdentifier,Status) :-
  p_get_table_for_identifier(TableIdentifier,Table),
  tbd_table_status_(Table,Status).

% Is also used in other predicates than tbd_table_status.
tbd_table_status_(table(_CallVariant,Status,_Trie,_Worklist),Status).
tbd_table_status_(complete_table(_,_),complete).

% PRIVATE
% Table must already exist.
p_get_table_for_identifier(TableIdentifier,Table) :-
  nb_getval(TableIdentifier,Table).

% Get the table identifier (!!) for call variant V, creating a new one if necessary.
%
% More costly than directly passing the table identifier for already existing tables.
%
% Since this creates a new table, this predicate is NOT meant for users who should get access to existing tables - f.e. benchmark shortest_path.P
%
table_for_variant(V,TableIdentifier) :-
  ( p_existing_table(V,TableIdentifier) ->
    true
  ;
    p_create_table(V,TableIdentifier)
  ).

% Get call variant for this table
get_call_variant(TableIdentifier,CallVariant) :-
  p_get_table_for_identifier(TableIdentifier,Table),
  get_call_variant_(Table,CallVariant).

get_call_variant_(table(CallVariant,_Status,_Trie,_Worklist),CallVariant).
get_call_variant_(complete_table(CallVariant,_AnswerTrie),CallVariant).

add_answer(TableIdentifier,A) :-
  p_get_table_for_identifier(TableIdentifier,Table),
% arg(1,Table,CallVariant),
  arg(3,Table,AnswerTrie),
  arg(4,Table,Worklist),
  duplicate_term(A,A2),
  % This predicate succeeds if the answer was new, otherwise it fails.
  trie_insert(AnswerTrie,A2,A2), % Use answer both as key and as value. Having it as value uses memory, but greatly simplifies getting all the answers.
  % We got here, so trie_insert added a new answer.
  % We must also insert this answer in the worklist
  wkl_add_answer(Worklist,A2).

get_answer(TableIdentifier,A) :-
  p_get_table_for_identifier(TableIdentifier,Table),
  get_answer_trie_(Table,AnswerTrie),
  % The trick is that we have stored the answers as values of the trie and that there is a method to get all the values.
  trie_get_all_values(AnswerTrie,A).

% get_answer_trie_(TableOrCompleteTable,AnswerTrie).
% First argument is not a TableIdentifier.
get_answer_trie_(table(_CallVariant,_Status,AnswerTrie,_Worklist),AnswerTrie).
get_answer_trie_(complete_table(_CallVariant,AnswerTrie),AnswerTrie).

% Get a list of answers for the given call variant.
% Used in compare_expected_for_variant/3 in testlib.pl
% IMPORTANT: table must be filled already, this is not done in this predicate! Therefore can be called during execution.
% V = variant
% LA = list of answers.
%
% More costly operation than directly giving the table identifier.
answers_for_variant(V,LA) :-
  table_for_variant(V,TableIdentifier),
  p_get_table_for_identifier(TableIdentifier,Table),
  get_answer_trie_(Table,AnswerTrie),
  findall(Value,trie_get_all_values(AnswerTrie,Value),LA).

% Set status of table TableIdentifier to active
set_active_status(TableIdentifier) :-
  tbd_status_transition(TableIdentifier,active,fresh,'set_active_status').

cleanup_after_complete(TableIdentifier) :-
  p_get_table_for_identifier(TableIdentifier,Table),
  cleanup_after_complete_(Table,TableIdentifier).

% Clause for a (noncomplete) table.
cleanup_after_complete_(
    table(CallVariant,_ActualOldStatus,AnswerTrie,_Worklist),
    TableIdentifier
  ) :-
  nb_linkval(TableIdentifier,complete_table(CallVariant,AnswerTrie)).
% If necessary for debugging add second clause for complete_table.

% Set status of table TableIdentifier to complete.
set_complete_status(TableIdentifier) :-
  % The transition must be active to complete, otherwise we have an invalid status transition.
  % Preexisting tables should have been cleaned-up, thus not have the form table/5 anymore, thus complete -> complete is not possible there.
  p_get_table_for_identifier(TableIdentifier,Table),
  set_complete_status_(Table,TableIdentifier).

% set_complete_status_(Table,TableIdentifier).
set_complete_status_(table(_CallVariant,_OldStatus,_AnswerTrie,_Worklist),TableIdentifier) :-
  tbd_status_transition(TableIdentifier,complete,active,'set_complete_status').

tbd_status_transition_no_check(TableIdentifier,NewStatus) :-
  p_get_table_for_identifier(TableIdentifier,Table),
  tbd_status_transition_no_check_(TableIdentifier,Table,NewStatus).

tbd_status_transition_no_check_(_TableIdentifier,Table,NewStatus) :-
    nb_setarg(2,Table,NewStatus).

% Set Table's status to NewStatus if current status is RequiredOldStatus, otherwise throw an exception mentioning CallerAsString: attempt to set NewStatus for table TableIdentifier, but current status was ActualOldStatus instead of RequiredOldStatus
tbd_status_transition(TableIdentifier,NewStatus,_RequiredOldStatus,_CallerAsString) :-
  p_get_table_for_identifier(TableIdentifier,Table),
  tbd_status_transition_no_check_(TableIdentifier,Table,NewStatus).

store_dependency(TableIdentifier,Suspension) :-
  p_get_table_for_identifier(TableIdentifier,table(_CallVariant,_Status,_AnswerTrie,Worklist)),
  copy_term(Suspension,SuspensionCopy),
  wkl_add_suspension(Worklist,SuspensionCopy).

% Get the worklist from the table identified by TableIdentifier
get_worklist(TableIdentifier,Worklist) :-
  p_get_table_for_identifier(TableIdentifier,Table),
  ( arg(4,Table,Worklist) ->
    true
  ;
    throw('get_worklist called on complete table!')
  ).
