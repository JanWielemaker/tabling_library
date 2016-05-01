:- ['format.pl'].

% Predicates of a general nature that may be useful in a lot of places.

print_trail_size(Message) :-
  write(Message), write(' '),
  print_trail_size.

% Debugging the trail stack
print_trail_size :-
  trail_size(S),
  format:format('Trail size is now ~w~n',[S]).

:- if(current_predicate(sysh:internalstat/5)).
trail_size(S) :-
  sysh:internalstat(3,_,S,_,x).
:- else.
trail_size(S) :-
  statistics(trailused, S).
:- endif.

% Utility function to construct internal sanity checks, only done in DEBUG mode.
% Throws an exception when called outside of DEBUG mode.
assert_initialized(InitializedCheckPredicate,CheckDescription,ContextualInfo) :-
  format:format('call to assert_initialized in nondebug mode: ~w~n',[CheckDescription]),
  throw('call to assert_initialized in nondebug mode').

formatln(X,Y) :-
  format:format(X,Y),
  write('\n').

assert_empty_list(ExpectedEmptyList,CallerErrorMessageForThrow,CallerGoalOnError) :-
  ( \+ ExpectedEmptyList \= [] ->
    true % is indeed empty
  ;
    call(CallerGoalOnError),
    throw(CallerErrorMessageForThrow)
  ).

% delete/3 with SWI-Prolog behaviour.
% delete/3 in library(lists) uses == to compare the elements
% example with a different behaviour: delete([p(a,_)],p(a,_),X).
delete_alt([], _,[]).
delete_alt([H|T], X, L) :-
  ( \+ X \= H ->
    delete_alt(T, X, L)
  ;
    L=[H|RT],
    delete_alt(T, X, RT)
  ).

assert_ground(X) :-
  ( ground(X) ->
    true
  ;
    format:format('BUG: ~w should have been ground, but it was not. Throwing exception~n',[X]),
    throw('BUG: got a nonground term, where we expected a ground one')
  ).

assert_nonvar(X) :-
  ( nonvar(X) ->
    true
  ;
    throw('BUG: we got a free variable, where we expected a nonvar')
  ).

% CustomException will be printed with format/2, and get no arguments.
assert_nonvar_with_custom_exception(X,CustomException) :-
  ( nonvar(X) ->
    true
  ;
    format:format(CustomException,[]),
    throw('BUG: we got a free variable, where we expected a nonvar')
  ).

assert_true(Goal) :-
  ( call(Goal) ->
    true
  ;
    format:format('ASSERTION FAILED: assert_true(~w)~n',[Goal]),
    throw('ASSERTION FAILED: we expected the Goal of assert_true to be true')
  ).

% Copy paste programming, but better error message
assert_false(Goal) :-
  ( call(Goal) ->
    format:format('ASSERTION FAILED: assert_false(~w)~n',[Goal]),
    throw('ASSERTION FAILED: we expected the Goal of assert_false to be false')
  ;
    true
  ).

% Perform goal for all existing tables, first printing
% >FOR EACH TABLE
% >END FOR EACH TABLE
% Goal should take 1 parameter: the name of the table.
foreach_table_with_print(Goal) :-
  % Prefixing with module name ('format:') is a hack. I suspect hProlog does not implement modules and meta_predicate completely correct.
  foreach_table(Goal,format:format('>FOR EACH TABLE\n',[]),format:format('>END FOR EACH TABLE\n', [])).

% Perform goal for all existing tables
% Goal should take 1 parameter: the name of the table.
foreach_table(Goal) :-
  foreach_table(Goal,true,true).

% Perform goal for all existing tables
% Goal should take 1 parameter: the name of the table.
% PreForeachGoal and PostForeachGoal should not take any more parameters.
% SWI-Prolog :- meta_predicate foreach_table(1,0,0).
foreach_table(Goal,PreForeachGoal,PostForeachGoal) :-
  call(PreForeachGoal),
  get_existing_tables(Ts),
  (
    member(T,Ts),
    call(Goal,T),
    fail
  ;
    call(PostForeachGoal)
  ).

test_map_component :-
  map_component(2,[1-a,2-b,3-c],L),
  format('test_map_component: ~w~n',[L]).

test_map_component2 :-
  map_component(2,[],L),
  format('test_map_component: ~w~n',[L]).

% Given a list of pairs, returns a list of the n-th components of the pairs where we start counting from 1
map_component(N,PairsList,ComponentList) :-
  % Partial application of arg/3
  maplist(arg(N),PairsList,ComponentList).

% Calls Goal/2 with arguments X and Y.
% Useful for partial application when the arguments of Goal are in the wrong order.
flip(Goal,Y,X) :-
  call(Goal,X,Y).

% any: Succeeds if Goal/1 succeeds for any element in the list
any(Goal,[El]) :- !,
  call(Goal,El).
any(Goal,[El|Rest]) :-
  (
    call(Goal,El), ! % If successful, cut choicepoint
  ;
    any(Goal,Rest)
  ).

% Expected: true; works fine
test_any_true_bool :-
  any_true_bool([false,false,false,true,false]).

% Expected: fail; works fine
test2_any_true_bool :-
  any_true_bool([false,false,false]).

% any_true_bool: Succeeds if at least one element of the list is the atom 'true'.
any_true_bool(List) :-
  any(is_true_bool,List).

% Helper predicate
is_true_bool(Bool) :-
  Bool = true.

% Testing purposes. Works fine.
test_predicate_list_to_tuple_list :-
  predicate_list_to_tuple_list([f(1,2),f(3,4)],TL),
  format('Test result: ~w~n',[TL]).

test2_predicate_list_to_tuple_list :-
  predicate_list_to_tuple_list([f(1,2)],TL),
  format('Test result: ~w~n',[TL]).

% Convert [f(X,Y,...), ...] to [X-Y-...,...]
% There is no check on the function symbol
predicate_list_to_tuple_list(PL,TL) :-
  maplist(predicate_to_tuple,PL,TL).

% f(X,Y) -> X-Y
% Does not work on facts.
predicate_to_tuple(P,T) :-
  P =.. [_Functor|Args],
  list_to_tuple(Args,T).

% [X,Y,Z] -> X-Y-Z
% Empty list. No sensible behaviour. Throw exception.
list_to_tuple([],_) :-
  throw('list_to_tuple called on empty list').
% List with at least one element.
list_to_tuple([First|Rest],Tuple) :-
  foldl(to_tuple,Rest,First,Tuple).

% E = 'element'
% Ai = 'accumulator in'
to_tuple(E,Ai,Ai-E).

% SWI-Prolog: available in autoloadable library(apply).
foldl(_FoldFunction,[],Zero,Zero) :- !.
foldl(FoldFunction,[X|Xs],InAcc,Result) :-
  call(FoldFunction,X,InAcc,TempAcc),
  foldl(FoldFunction,Xs,TempAcc,Result).

  % Succeeds if L1 and L2 have the same length and their elements are pairwise unifiable.
  % Unificiation on the elements is undone after the test (by using \+ \=)
  % Test results: seems to work OK.
equal_lists([],[]).
equal_lists([E1|R1],[E2|R2]) :-
  \+ E1 \= E2,
  equal_lists(R1,R2).

% Expect true.
test_equal_lists1 :-
  equal_lists([1,2,3],[1,2,3]).

% Expect false.
test_equal_lists2 :-
  equal_lists([1,2,3],[1,2,4]).

% Expect false.
test_equal_lists3 :-
  equal_lists([1,2,3],[1,2]).

% Expect true.
test_equal_lists4 :-
  equal_lists([_X,2],[_Y,2]).
