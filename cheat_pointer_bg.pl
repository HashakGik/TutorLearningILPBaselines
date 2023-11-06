% CHEAT PREDICATES FOR EASY CURRICULUM:

symmetric_list(L) :- reverse(L, L).

% CHEAT PREDICATES FOR MEDIUM CURRICULUM:

forall_same_color(C, CO) :- forall(contains(C, C1), (contains(C1, C2), extract_color(C2, CO))).
forall_same_shape(C, SH) :- forall(contains(C, C1), (contains(C1, C2), extract_color(C2, SH))).
extract_two_different(C, C1, C2) :- contains(C, C1), contains(C, C2), C1 \= C2.

% CHEAT PREDICATES FOR HARD CURRICULUM:

perpendicular(C) :- defined_as(C, stack, _), contains(C, C1), defined_as(C1, side_by_side, _).
perpendicular(C) :- defined_as(C, side_by_side, _), contains(C, C1), defined_as(C1, stack, _).

tmp_palindrome(C) :- atomic_obj(C).
tmp_palindrome(C) :- defined_as(C, _, L), reverse(L, L).
forall_palindrome(L) :- forall(member(C1, L), tmp_palindrome(C1)).

pseudo_palindrome([]).
pseudo_palindrome([_]).
pseudo_palindrome(L) :- middle(L,M),pseudo_palindrome(M),last(L,A),first(L,B), same_shape(_, [A,B]).
pseudo_palindrome(L) :- middle(L,M),pseudo_palindrome(M),last(L,A),first(L,B), same_color(_, [A,B]).
pseudo_palindrome(L) :- middle(L,M),pseudo_palindrome(M),last(L,A),first(L,A), defined_as(A, _, _).

forall_palindrome2(L) :- forall(member(C1, L), forall_palindrome(C1)).

minimum_count_recursive([H|_], H, 1) :- atomic_obj(H).
minimum_count_recursive([C], C, N) :- defined_as(C, _, L), length(L, N).
minimum_count_recursive([H|T], H, N) :- defined_as(H, _, L), length(L, N), minimum_count_recursive(T, _, N1), less_eq(N, N1).
minimum_count_recursive([H|T], X, N1) :- defined_as(H, _, L), length(L, N), minimum_count_recursive(T, X, N1), greater(N, N1).

minimum_count_number(L, N) :- minimum_count_recursive(L, _, N).
minimum_count_child(L, T) :- minimum_count_recursive(L, T, _).


exists_red_square([X|_]) :- extract_shape(X, square), extract_color(X, red).
exists_red_square([_|T]) :- exists_red_square(T).

rotate([A, B, C, D], [C, A, D, B]).

majority_color(CO, L) :- nth0(I, L, X1), nth0(J, L, X2), different_int(I, J), same_color(CO, [X1, X2]).
majority_shape(SH, L) :- nth0(I, L, X1), nth0(J, L, X2), different_int(I, J), same_shape(SH, [X1, X2]).


