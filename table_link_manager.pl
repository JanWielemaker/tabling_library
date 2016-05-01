:- ensure_loaded(['trie.pl']).
:- ensure_loaded(['utils.pl']).

:- ensure_loaded(['format.pl']).

% This file defines a call pattern trie.
%
% This data structure keeps the relation between a variant and the corresponding table identifier using a trie. The trick is to make a canonical representation of a given variant using the numbervars/3 predicate. The trie uses this canonical representation as key, and the table identifier as value.

% Uses the (private) global variable trie_table_link

% This predicate should be called exactly once.
% It throws an exception if it is called more than once.
table_link_manager_initialize :-
  ( table_link_manager_initialized ->
    throw('table_link_manager_initialize: already initialized: 2nd call not allowed')
  ;
    trie_new(EmptyTrie),
    nb_linkval(trie_table_link,EmptyTrie)
  ).

% Succeeds if the table_link_manager_initialize/0 predicate was already called.
table_link_manager_initialized :-
  \+ nb_getval(trie_table_link,[]).

% PRIVATE
% mode: + -
% Variant is not modified
variant_canonical_representation(Variant,CanonicalRepresentation) :-
  copy_term(Variant,CanonicalRepresentation),
  numbervars(CanonicalRepresentation,0,_N).

% Succeeds if there is a table TableIdentifier in existance for the given call variant Variant.
p_existing_table(Variant,TableIdentifier) :-
  nb_getval(trie_table_link,Trie),
  variant_canonical_representation(Variant,CanonicalRepresentation),
  trie_lookup(Trie,CanonicalRepresentation,TableIdentifier).

% Important remark: we cannot use an out-of-the-box association list, because we need a lookup based on variant checking, which is not available for such lists. Converting the association list to a regular list => why would you use an association list in the first place...
p_link_variant_identifier(Variant,TableIdentifier) :-
  nb_getval(trie_table_link,Trie),
  variant_canonical_representation(Variant,CanonicalRepresentation),
  trie_insert_succeed(Trie,CanonicalRepresentation,TableIdentifier),
  nb_linkval(trie_table_link,Trie).

% Returns a list of existing table identifiers.
% Rather costly.
get_existing_tables(Ts) :-
  nb_getval(trie_table_link,Trie),
  findall(T,trie_get_all_values(Trie,T),Ts).

% A very unefficient way of implementing this predicate. But it is only used for unit testing, so it doesn't really matter.
% Also, it doesn't require any additional bookkeeping during the actual execution.
num_tables(N) :-
  get_existing_tables(Ts),
  length(Ts,N).
