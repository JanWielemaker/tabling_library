% A circular double linked list
% =============================

% Always have a unused-cell at the beginning.

% I do not always inline unifications because the head is then more readable for users who don't need to know the details.

% Due to lack of modules in hProlog, the following predicate names should not be used elsewhere:
% - the heads of all following rules (starting with dll_, I reserve "the namespace"!)

% dll_cell(Element,Next,Previous)

% The following is perhaps odd:
%
%   Next link = more to the front (the left)
%   Previous link = more to the back (the right)
%
%   List structure
%   --------------
%   front-of-the-list | ... | back-of-the-list

:- ensure_loaded(['format.pl']). % hProlog-specific

:- ensure_loaded(['utils.pl']).

dll_new_double_linked_list(List) :-
  % Nonused cell dll_start at the beginning, points to itself (this is easy when adding elements).
  List = dll_cell(dll_start,List,List).

dll_append_right(List,Element) :-
  dll_append_right(List,Element,_Pointer).

dll_append_left(List,Element) :-
  dll_append_left(List,Element,_Pointer).

% Append at the back of the list
% Mode: + + -
dll_append_right(List,Element,Pointer) :-
  % Get pointer to cell currently at the back. Done by taking the previous element from the unused element representing the list.
  dll_get_pointer_to_previous(List,OldBack),
  % Make the new cell point to OldBack as predecessor
  % Make the new cell point to the unused cell as successor.
  Pointer = dll_cell(Element,List,OldBack),
  % Make OldBack point to the new cell as successor
  dll_p_set_next_pointer(OldBack,Pointer),
  % Make the unused cell point to the new cell as predecessor
  dll_p_set_previous_pointer(List,Pointer).

% Add to the front of the list
% Mode: + + -
dll_append_left(List,Element,Pointer) :-
  % Get pointer to cell currently at the front. Done by taking the next element from the unused element representing the list.
  dll_get_pointer_to_next(List,OldFront),
  % Make the new cell point to OldFront as successor
  % Make the new cell point to the unused cell as predecessor
  Pointer = dll_cell(Element,OldFront,List),
  % Make OldFront point to the new cell as predecessor
  dll_p_set_previous_pointer(OldFront,Pointer),
  % Make the unused cell point to the new cell as successor
  dll_p_set_next_pointer(List,Pointer).

% get_next_cell?
dll_get_pointer_to_next(dll_cell(_Data,PointerNext,_PointerPrevious),PointerNext).

% get_previous_cell?
dll_get_pointer_to_previous(dll_cell(_Data,_PointerNext,PointerPrevious),PointerPrevious).

% Will happily give you the "data" from the unused cell at the beginning. (We use this odd behaviour below, f.e. in dll_p_foreach_element_/2.)
dll_get_data(dll_cell(Data,_PointerNext,_PointerPrevious),Data).

dll_is_dummy_pointer(List,Pointer) :-
  \+ Pointer \= List.

% Special case of swapping - used in dll_swap/2.
% This is also the case used for swapping a freshly created list with itself.
%
% Sketch: APrevious <-> PointerA <-> PointerB <-> BNext etc.
dll_p_swap_adjacent_elements(PointerA,PointerB) :-
  % Order B A?
  ( dll_get_pointer_to_next(PointerB,PointerA) ->
    dll_p_swap_adjacent_elements_(PointerB,PointerA)
  ;
    % Order A B!
    dll_p_swap_adjacent_elements_(PointerA,PointerB)
  ).

% Assumes the order A B.
dll_p_swap_adjacent_elements_(PointerA,PointerB) :-
  % Get A's previous and B's next
  dll_get_pointer_to_previous(PointerA,PointerAPrevious),
  dll_get_pointer_to_next(PointerB,PointerBNext),
  % Set A's previous to B
  dll_p_set_previous_pointer(PointerA,PointerB),
  % Set B's next to A
  dll_p_set_next_pointer(PointerB,PointerA),
  % Set A's next to BNext
  dll_p_set_next_pointer(PointerA,PointerBNext),
  % Set B's previous to APrevious
  dll_p_set_previous_pointer(PointerB,PointerAPrevious),
  % Set APrevious' next to B !!
  dll_p_set_next_pointer(PointerAPrevious,PointerB),
  % Set BNext's previous to A !!
  dll_p_set_previous_pointer(PointerBNext,PointerA).

% Private
% Careful: make sure this is called on the actual cell, and not some copy.
% Mode: + +
dll_p_set_previous_pointer(Cell,PointerToNewPrevious) :-
  nb_linkarg(3,Cell,PointerToNewPrevious).

% Private
% Careful: make sure this is called on the actual cell, and not some copy.
% Mode: + +
dll_p_set_next_pointer(Cell,PointerToNewNext) :-
  nb_linkarg(2,Cell,PointerToNewNext).
