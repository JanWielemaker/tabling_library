% A batched worklist: a worklist that clusters suspensions and answers as much as possible.
% The idea is to minimize the number of swaps. This should be more efficient than the worklist implementation without clustering.

% Argument positions for nb_setarg:
% 1: double linked list
% 2: pointer to the list entry of the rightmost inner answer cluster
% 3: flag indicating the execution of wkl_unfolded_do_all_work
% 4: flag indicating whether the table identifier associated with this worklist is already in the global worklist. This is because more than one answer can be added due to the execution of other worklists.
% 5: table identifier for the table this worklist belongs to

% Contents of a batched worklist:
% wkl_answer_cluster([Answer|RestAnswers]).
% wkl_suspension([Suspension|RestSuspension]).

% The difficulty is that you should not add new entries to a cluster once you started its execution. Probably the simplest way to do so is by swapping the answer cluster AC and suspension cluster SC before you take the cartesian product of all answers in AC with all suspensions in SC.

% Illustration why you may need a complex procedure for finding the future rightmost inner answer cluster.
% Assume all clusters have 2 entries.
% 1) AA1 CC1
% 2) AA2 CC1 AA1 CC2 (swapped AA1 and CC1)
% 3) AA2 CC1 CC2 AA1 (swapped AA1 and CC2)
% 4) AA3 CC1 AA2 CC2 AA1 CC3 (swapped AA2 and CC1)
%
% Now AA1 is the RIAC, but AA2 is the future RIAC.
%
% Can you find the future RIAC smarter than by walking back? If you don't, then it doesn't make sense to use a future RIAC at all. You could use a stack, which should not grow too large because you use batches. But walking back also should not take too long, since you use batches.

% So let's not use a future RIAC in the first place, and just walk back when we need a new RIAC. This is easy to implement, hence we can test more quickly.

% Abbreviations:
% RIAC = rightmost inner answer cluster
% FUTRIAC = future rightmost inner answer cluster

:- ['double_linked_list.pl'].
:- ['globalWorklist.pl'].

% Get a new empty worklist.
wkl_new_worklist(TableIdentifier, wkl_worklist(List,List,false,true,TableIdentifier)) :-
  dll_new_double_linked_list(List),
  % We set the RIAC to the dummy element at the start of the double linked list, which is List.
  % Don't set all the rest for now.
  add_to_global_worklist(TableIdentifier).

% The work is done if the RIAC pointer points to the unused cell at the beginning.
% The work is also done if the RIAC pointer points to the sole answer cluster in a list dll_start - wkl_answer_cluster, because in that case there are no suspensions to swap with. This is a special case, which we only discovered by testing. You can detect it by checking whether the NEXT-pointer of the RIAC is the dummy pointer.
wkl_worklist_work_done(Worklist) :-
  wkl_p_get_rightmost_inner_answer_cluster_pointer(Worklist,RiacPointer),
  ( wkl_is_dummy_pointer(Worklist,RiacPointer) ->
    true
  ;
    dll_get_pointer_to_next(RiacPointer,NextPointer),
    wkl_is_dummy_pointer(Worklist,NextPointer)
  ).

set_flag_executing_all_work(Worklist) :-
  nb_setarg(3,Worklist,true).

unset_flag_executing_all_work(Worklist) :-
  nb_setarg(3,Worklist,false).

% Swap answer cluster and the adjacent continuation cluster.
% Mode: + + -
wkl_p_swap_answer_continuation(Worklist,InnerAnswerClusterPointer,SuspensionClusterPointer) :-
  % You can have a worklist containing only an answer cluster, but no continuations.
  % In that case SuspensionClusterPointer will be dll_start. We must take our precautions elsewhere.
  % Do not forget that the list of answers and the list of suspensions is wrapped in a predicate!
  dll_get_pointer_to_next(InnerAnswerClusterPointer,SuspensionClusterPointer),
  % For reasons of speed we don't use dll_swap: we only swap adjacent elements and we can be sure that they are in the order A,B.
  % Therefore we can use dll_p_swap_adjacent_elements_
  dll_p_swap_adjacent_elements_(InnerAnswerClusterPointer,SuspensionClusterPointer),
  % Update the necessary pointers
  wkl_p_update_righmost_inner_answer_cluster_pointer(Worklist,InnerAnswerClusterPointer).

% Update the pointer if the answer cluster it points to is no longer the rightmost inner answer cluster.
wkl_p_update_righmost_inner_answer_cluster_pointer(Worklist,InnerAnswerClusterPointer) :-
  ( wkl_p_answer_cluster_currently_moved_completely(Worklist,InnerAnswerClusterPointer) ->
    wkl_p_find_new_rightmost_inner_answer_cluster_pointer(Worklist,InnerAnswerClusterPointer,NewRiacPointer),
    wkl_p_set_rightmost_inner_answer_cluster_pointer(Worklist,NewRiacPointer)
  ;
    true
  ).

% Rationale for this implementation: see the top of the file.
% Unify NewRiacPointer to the first pointer satisfying the following conditions:
% - left of StartPointer (when viewing the list as DUMMY-ELEM POINTER POINTER POINTER START-POINTER)
% - either an anwer pointer or the dummy element
% When StartPointer is the dummy element, NewRiacPointer is also the dummy element. We never look "in front of" the dummy element.
wkl_p_find_new_rightmost_inner_answer_cluster_pointer(Worklist,StartPointer,NewRiacPointer) :-
  ( wkl_is_dummy_pointer(Worklist,StartPointer) ->
    NewRiacPointer = StartPointer
  ;
    dll_get_pointer_to_previous(StartPointer,FirstCandidatePointer),
    wkl_p_find_new_riac_helper(Worklist,FirstCandidatePointer,NewRiacPointer)
  ).

wkl_p_find_new_riac_helper(Worklist,CandidatePointer,NewRiacPointer) :-
  ( is_answer_cluster_or_dummy_pointer(Worklist,CandidatePointer) ->
    NewRiacPointer = CandidatePointer
  ;
    dll_get_pointer_to_previous(CandidatePointer,NewCandidate),
    wkl_p_find_new_riac_helper(Worklist,NewCandidate,NewRiacPointer)
  ).
  
is_answer_cluster_or_dummy_pointer(Worklist,Pointer) :-
  ( wkl_is_dummy_pointer(Worklist,Pointer) ->
    true
  ;
    wkl_p_dereference_pointer(Worklist,Pointer,A),
    wkl_p_is_answer_cluster(A)
  ).

% Failure-driven loop
wkl_clusters_cartesian_product(AnswerCluster,SuspensionCluster) :-
  ( member(Answer,AnswerCluster),
    member(Suspension,SuspensionCluster),
    % The meat
    run_worklist_helper(Suspension,Answer),
    % Trigger loop
    fail
  ;
    % Loop base case
    true
  ).

wkl_both_flags_unset(wkl_worklist(_Dll,_Riac,false,false,_TableIdentifier)).

set_global_worklist_presence_flag(Worklist) :-
  nb_setarg(4,Worklist,true).

unset_global_worklist_presence_flag(Worklist) :-
  nb_setarg(4,Worklist,false).

potentially_add_to_global_worklist(Worklist) :-
  ( wkl_both_flags_unset(Worklist) ->
    % Set the flag for presence in the metaworklist
    set_global_worklist_presence_flag(Worklist),
    % Should add to the metaworklist
    arg(5,Worklist,TableIdentifier),
    add_to_global_worklist(TableIdentifier)
  ;
    % Nothing to do.
    true
  ).

wkl_add_answer(Worklist,Answer) :-
  % Add to global worklist if not executing during wkl_unfolded_do_all_work and not there yet as well.
  potentially_add_to_global_worklist(Worklist),
  ( wkl_p_leftmost_cluster_is_answer_cluster(Worklist) ->
    wkl_add_to_existing_answer_cluster(Worklist,Answer)
    % If you add to an existing cluster, then obviously you should not change the RIAC.
  ;
    wkl_add_to_new_answer_cluster(Worklist,Answer,AnswerClusterPointer),
    % If the RIAC is the dummy pointer, we need to change that.
    wkl_p_update_rightmost_inner_answer_cluster_pointer(Worklist,AnswerClusterPointer) 
  ).

wkl_p_update_rightmost_inner_answer_cluster_pointer(Worklist,NewAnswerClusterPointer) :-
  wkl_p_get_rightmost_inner_answer_cluster_pointer(Worklist,CurrentRiac),
  ( wkl_is_dummy_pointer(Worklist,CurrentRiac) ->
    wkl_p_set_rightmost_inner_answer_cluster_pointer(Worklist,NewAnswerClusterPointer)
  ;
    % Nothing to do.
    true
  ).

wkl_add_suspension(Worklist,Suspension) :-
  % Add to global worklist if not executing during wkl_unfolded_do_all_work and not there yet as well.
  potentially_add_to_global_worklist(Worklist),
  ( wkl_p_rightmost_cluster_is_suspension_cluster(Worklist) ->
    wkl_add_to_existing_suspension_cluster(Worklist,Suspension)
  ;
    wkl_add_to_new_suspension_cluster(Worklist,Suspension,SuspensionClusterPointer),
    % If added to a new suspension cluster, we may need to change the righmost inner answer pointer
    wkl_p_potential_rias_update_add_contin(Worklist,SuspensionClusterPointer)
  ).

% This predicate should not fail.
wkl_p_potential_rias_update_add_contin(Worklist,SuspensionClusterPointer) :-
  % Look back one entry of the freshly inserted SuspensionClusterPointer
  dll_get_pointer_to_previous(SuspensionClusterPointer,PotentialNewRiacPointer),
  ( wkl_p_is_answer_cluster_pointer(Worklist,PotentialNewRiacPointer) ->
    % We must indeed update the rightmost inner answer cluster pointer.
    wkl_p_set_rightmost_inner_answer_cluster_pointer(Worklist,PotentialNewRiacPointer)
  ;
    % Nothing to do, but we should not fail.
    true
  ).

wkl_add_to_existing_answer_cluster(Worklist, Answer) :-
  arg(1,Worklist,Dll),
  dll_get_pointer_to_next(Dll,AnswerClusterPointer),
  wkl_p_dereference_pointer(Worklist,AnswerClusterPointer,AnswerCluster),
  AnswerCluster = wkl_answer_cluster(AnswersAlreadyInCluster),
  nb_setarg(1,AnswerCluster,[Answer|AnswersAlreadyInCluster]). 
  
wkl_add_to_new_answer_cluster(
    wkl_worklist(Dll,_Ria,_FlagExecutingWork,_AlreadyInMetaworklist,_TableIdentifier),
    Answer,AnswerClusterPointer
  ) :-
  dll_append_left(Dll,wkl_answer_cluster([Answer]),AnswerClusterPointer).

wkl_add_to_existing_suspension_cluster(Worklist, Suspension) :-
  arg(1,Worklist,Dll),
  dll_get_pointer_to_previous(Dll,SuspensionClusterPointer),
  wkl_p_dereference_pointer(Worklist,SuspensionClusterPointer,SuspensionCluster),
  SuspensionCluster = wkl_suspension_cluster(SuspensionsAlreadyInCluster),
  nb_setarg(1,SuspensionCluster,[Suspension|SuspensionsAlreadyInCluster]).

wkl_add_to_new_suspension_cluster(
    wkl_worklist(Dll,_Ria,_FlagExecutingWork,_AlreadyInMetaworklist,_TableIdentifier),
    Suspension,
    SuspensionClusterPointer
  ) :-
  dll_append_right(Dll,wkl_suspension_cluster([Suspension]),SuspensionClusterPointer).

wkl_p_is_answer_cluster(CandidateAnswerCluster) :-
  nonvar(CandidateAnswerCluster),
  CandidateAnswerCluster = wkl_answer_cluster(_).

wkl_p_is_suspension_cluster(CandidateSuspensionCluster) :-
  nonvar(CandidateSuspensionCluster),
  CandidateSuspensionCluster = wkl_suspension_cluster(_).

wkl_p_leftmost_cluster_is_answer_cluster(Worklist) :-
  arg(1,Worklist,Dll), 
  dll_get_pointer_to_next(Dll,CandidateAnswerClusterPointer),
  wkl_p_is_answer_cluster_pointer(Worklist,CandidateAnswerClusterPointer).

wkl_p_rightmost_cluster_is_suspension_cluster(Worklist) :-
  arg(1,Worklist,Dll), 
  dll_get_pointer_to_previous(Dll,CandidateSuspensionClusterPointer),
  wkl_p_is_suspension_cluster_pointer(Worklist,CandidateSuspensionClusterPointer).


wkl_p_get_rightmost_inner_answer_cluster_pointer(wkl_worklist(_Dll,InnerAnswerClusterPointer,_FlagExecutingWork,_AlreadyInMetaworklist,_TableIdentifier), InnerAnswerClusterPointer).

% Succeed if there are currently no more continuation clusters on the right of the given position:
% Why 'currently' in the name? Another continuation can be added.
wkl_p_answer_cluster_currently_moved_completely(Worklist,AnswerClusterPointer) :-
  ( wkl_p_at_right(Worklist,AnswerClusterPointer) ->
    true
  ;
    wkl_p_answer_cluster_on_right(Worklist,AnswerClusterPointer)
  ).

% Succeeds if the given pointer points to the last element in the list. That is, if its next pointer is the dummy element in the double linked list.
wkl_p_at_right(Worklist,Pointer) :-
  dll_get_pointer_to_next(Pointer,NextPointer),
  wkl_is_dummy_pointer(Worklist,NextPointer).
  
wkl_p_answer_cluster_on_right(Worklist,Pointer) :-
  dll_get_pointer_to_next(Pointer,NextPointer),
  wkl_p_is_answer_cluster_pointer(Worklist,NextPointer).

wkl_is_dummy_pointer(Worklist,Pointer) :-
  wkl_p_get_double_linked_list(Worklist,Dll),
  dll_is_dummy_pointer(Dll,Pointer).

wkl_p_is_answer_cluster_pointer(Worklist,PointerCandidateAnswerCluster) :-
  ( wkl_is_dummy_pointer(Worklist,PointerCandidateAnswerCluster) ->
    % Certainly not an answer cluster, should not dereference this
    fail
  ;
    wkl_p_dereference_pointer(Worklist,PointerCandidateAnswerCluster,CandidateAnswerCluster),
    wkl_p_is_answer_cluster(CandidateAnswerCluster)
  ).

wkl_p_is_suspension_cluster_pointer(Worklist,PointerCandidateSuspensionCluster) :-
  ( wkl_is_dummy_pointer(Worklist,PointerCandidateSuspensionCluster) ->
    % Certainly not an answer cluster, should not dereference this
    fail
  ;
    wkl_p_dereference_pointer(Worklist,PointerCandidateSuspensionCluster,CandidateSuspensionCluster),
    wkl_p_is_suspension_cluster(CandidateSuspensionCluster)
  ).

wkl_p_get_double_linked_list(Worklist,Dll) :-
  arg(1,Worklist,Dll).

% One should not attempt to dereference the dummy pointer in the double linked list.
wkl_p_dereference_pointer(_Worklist,Pointer,Data) :-
  dll_get_data(Pointer,Data).

% SETTING POINTERS
%%%%%%%%%%%%%%%%%%

wkl_p_set_rightmost_inner_answer_cluster_pointer(Worklist,AnswerClusterPointer) :-
  nb_setarg(2,Worklist,AnswerClusterPointer).
