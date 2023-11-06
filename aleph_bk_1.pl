:- use_module(aleph).
:- if(current_predicate(use_rendering/1)).
:- use_rendering(prolog).
:- endif.
:- aleph.
:-style_check(-discontiguous).
:- aleph_set(verbosity, 0).
:- aleph_set(newvars, 20).
:- aleph_set(clauses, 1).
:- aleph_set(clauselength, 6).
:- aleph_set(min_acc, 0.5).
:- aleph_set(noise, 0).
:- aleph_set(i, 2).
:- aleph_set(min_pos, 2).
:- aleph_set(depth, 10).
:- aleph_set(nodes, 5000).
:- aleph_set(timeout, 60).
:- modeh(*, valid(+sample)).

%:- determination(valid/1, red/1).
%:- determination(valid/1, green/1).
%:- determination(valid/1, blue/1).
%:- determination(valid/1, cyan/1).
%:- determination(valid/1, magenta/1).
%:- determination(valid/1, yellow/1).
%:- determination(valid/1, small/1).
%:- determination(valid/1, large/1).
%:- determination(valid/1, triangle/1).
%:- determination(valid/1, circle/1).
%:- determination(valid/1, square/1).

%:- determination(valid/1, random/1).
%:- determination(valid/1, stack/1).
%:- determination(valid/1, grid/1).
%:- determination(valid/1, stack_reduce_bb/1).
%:- determination(valid/1, side_by_side/1).
%:- determination(valid/1, side_by_side_reduce_bb/1).
%:- determination(valid/1, diag_ul_lr/1).
%:- determination(valid/1, diag_ll_ur/1).


%:- determination(valid/1, color/1).
%:- determination(valid/1, shape/1).
%:- determination(valid/1, size/1).

%:- determination(valid/1, non_diag/1).
%:- determination(valid/1, diag/1).

%:- determination(valid/1, non_random/1).
%:- determination(valid/1, any_composition/1).
%:- determination(valid/1, line/1).

:- determination(valid/1, sample_is/2).

:- determination(valid/1, extract_shape/2).
:- determination(valid/1, extract_color/2).
:- determination(valid/1, extract_size/2).

:- determination(valid/1, exists_shape/2).
:- determination(valid/1, exists_color/2).
:- determination(valid/1, exists_size/2).

:- determination(valid/1, same_shape/2).
:- determination(valid/1, same_color/2).
:- determination(valid/1, same_size/2).

:- determination(valid/1, contains/2).
:- determination(valid/1, recursive_contains/2).
:- determination(valid/1, contains_composition/2).
:- determination(valid/1, contains_composition_depth/3).

:- determination(valid/1, defined_as/3).
%:- determination(valid/1, extract_operator/2).
%:- determination(valid/1, extract_children/2).
%:- determination(valid/1, extract_op_and_chld/3).

:- determination(valid/1, same_attribute/1).
:- determination(valid/1, same_non_size/1).
:- determination(valid/1, all_same/1).

:- determination(valid/1, first/2).
:- determination(valid/1, last/2).
:- determination(valid/1, prepend/3).
:- determination(valid/1, droplast/2).
:- determination(valid/1, middle/2).
:- determination(valid/1, getmiddle/2).
:- determination(valid/1, dropmiddle/2).

:- determination(valid/1, less/2).
:- determination(valid/1, less_eq/2).
:- determination(valid/1, greater/2).
:- determination(valid/1, same_int/2).
:- determination(valid/1, different_int/2).
:- determination(valid/1, same_obj/2).
:- determination(valid/1, different_obj/2).

%:- determination(valid/1, atom/1).
:- determination(valid/1, reverse/2).
:- determination(valid/1, length/2).
:- determination(valid/1, delete/3).
:- determination(valid/1, nth0/3).
:- determination(valid/1, member/2).

:- modeb(*, sample_is(+sample, -term_t)).

%:- modeb(*, red(#color_t)).
%:- modeb(*, green(#color_t)).
%:- modeb(*, blue(#color_t)).
%:- modeb(*, cyan(#color_t)).
%:- modeb(*, magenta(#color_t)).
%:- modeb(*, yellow(#color_t)).
%:- modeb(*, small(#size_t)).
%:- modeb(*, large(#size_t)).
%:- modeb(*, triangle(#shape_t)).
%:- modeb(*, circle(#shape_t)).
%:- modeb(*, square(#shape_t)).

%:- modeb(*, random(#comp_op)).
%:- modeb(*, stack(#comp_op)).
%:- modeb(*, grid(#comp_op)).
%:- modeb(*, stack_reduce_bb(#comp_op)).
%:- modeb(*, side_by_side(#comp_op)).
%:- modeb(*, side_by_side_reduce_bb(#comp_op)).
%:- modeb(*, diag_ul_lr(#comp_op)).
%:- modeb(*, diag_ll_ur(#comp_op)).

%:- modeb(*, color(#color_t)).
%:- modeb(*, shape(#shape_t)).
%:- modeb(*, size(#size_t)).

%:- modeb(*, non_diag(#comp_op)).
%:- modeb(*, diag(#comp_op)).

%:- modeb(*, non_random(#comp_op)).
%:- modeb(*, any_composition(#comp_op)).
%:- modeb(*, line(#comp_op)).

:- modeb(*, extract_shape(+term_t, #shape_t)).
:- modeb(*, extract_color(+term_t, #color_t)).
:- modeb(*, extract_size(+term_t, #size_t)).

:- modeb(*, exists_shape(#shape_t, +list_t)).
:- modeb(*, exists_color(#color_t, +list_t)).
:- modeb(*, exists_size(#size_t, +list_t)).

:- modeb(*, same_shape(#shape_t, +list_t)).
:- modeb(*, same_color(#color_t, +list_t)).
:- modeb(*, same_size(#size_t, +list_t)).

:- modeb(*, contains(+term_t, -term_t)).
:- modeb(*, recursive_contains(+term_t, -term_t)).
:- modeb(*, contains_composition(+term_t, #comp_op)).
:- modeb(*, contains_composition_depth(+term_t, #comp_op, -int)).

:- modeb(*, defined_as(+term_t, #comp_op, -list_t)).
%:- modeb(*, extract_operator(+term_t, #comp_op)).
%:- modeb(*, extract_children(+term_t, -list_t)).
%:- modeb(*, extract_op_and_chld(+term_t, #comp_op, -list_t)).


:- modeb(*, same_attribute(+list_t)).
:- modeb(*, same_non_size(+list_t)).
:- modeb(*, all_same(+list_t)).

:- modeb(*, first(+list_t, -term_t)).
:- modeb(*, last(+list_t, -term_t)).
:- modeb(*, prepend(+, +list_t, -list_t)).
:- modeb(*, droplast(+list_t, -list_t)).
:- modeb(*, middle(+list_t, -list_t)).
:- modeb(*, getmiddle(+list_t, -term_t)).
:- modeb(*, dropmiddle(+list_t, -list_t)).

:- modeb(*, less(+int, +int)).
:- modeb(*, less_eq(+int, +int)).
:- modeb(*, greater(+int, +int)).
:- modeb(*, same_int(+int, +int)).
:- modeb(*, different_int(+int, +int)).
:- modeb(*, same_obj(+term_t, +term_t)).
:- modeb(*, different_obj(+term_t, +term_t)).

%:- modeb(*, atom(+term_t)).
:- modeb(*, reverse(+list_t, -list_t)).
:- modeb(*, length(+list_t, -int)).
:- modeb(*, delete(+list_t, +term_t, -list_t)).
:- modeb(*, nth0(+int, +list_t, -term_t)).
:- modeb(*, member(-term_t, +)).


:- modeb(*, symmetric_list(+list_t)).

:- modeb(*, forall_same_color(+term_t, #color_t)).
:- modeb(*, forall_same_shape(+term_t, #shape_t)).
:- modeb(*, extract_two_different(+term_t, -term_t, -term_t)).

:- modeb(*, perpendicular(+term_t)).
:- modeb(*, forall_palindrome(+list_t)).
:- modeb(*, pseudo_palindrome(+list_t)).
:- modeb(*, forall_palindrome2(+list_t)).
:- modeb(*, minimum_count_number(+list_t,-int)).
:- modeb(*, minimum_count_child(+list_t,-term_t)).
:- modeb(*, exists_red_square(+list_t)).
:- modeb(*, rotate(+list_t,-list_t)).
:- modeb(*, majority_color(#color_t,+list_t)).
:- modeb(*, majority_shape(#shape_t,+list_t)).

:- determination(valid/1, symmetric_list/1).

:- determination(valid/1, forall_same_color/2).
:- determination(valid/1, forall_same_shape/2).
:- determination(valid/1, extract_two_different/3).


:- determination(valid/1, perpendicular/1).
:- determination(valid/1, forall_palindrome/1).
:- determination(valid/1, pseudo_palindrome/1).
:- determination(valid/1, forall_palindrome2/1).
:- determination(valid/1, minimum_count_number/2).
:- determination(valid/1, minimum_count_child/2).
:- determination(valid/1, exists_red_square/1).
:- determination(valid/1, rotate/2).
:- determination(valid/1, majority_color/2).
:- determination(valid/1, majority_shape/2).

:-begin_bg.
red(red).
green(green).
blue(blue).
cyan(cyan).
magenta(magenta).
yellow(yellow).
triangle(triangle).
circle(circle).
square(square).
small(small).
large(large).


color(X) :- red(X).
color(X) :- green(X).
color(X) :- blue(X).
color(X) :- cyan(X).
color(X) :- magenta(X).
color(X) :- yellow(X).

shape(X) :- triangle(X).
shape(X) :- circle(X).
shape(X) :- square(X).

size(X) :- small(X).
size(X) :- large(X).

stack(stack).
side_by_side(side_by_side).
stack_reduce_bb(stack_reduce_bb).
side_by_side_reduce_bb(side_by_side_reduce_bb).
grid(grid).
diag_ul_lr(diag_ul_lr).
diag_ll_ur(diag_ll_ur).
random(random).



non_diag(X) :- stack(X).
non_diag(X) :- side_by_side(X).
non_diag(X) :- stack_reduce_bb(X).
non_diag(X) :- side_by_side_reduce_bb(X).
non_diag(X) :- grid(X).

diag(X) :- diag_ul_lr(X).
diag(X) :- diag_ll_ur(X).

non_random(X) :- diag(X).
non_random(X) :- non_diag(X).
any_composition(X) :- non_random(X).
any_composition(X) :- random(X).

line(X) :- non_random(X), not(grid(X)).

shape_props(T, SH, CO, SZ) :- atomic_obj(T), term_string(T, S), split_string(S, "_", "", L), L = [SH, CO, SZ].
extract_shape(T, SH) :- shape_props(T, SH1, _, _), term_string(SH, SH1), shape(SH).
extract_color(T, CO) :- shape_props(T, _, CO1, _), term_string(CO, CO1), color(CO).
extract_size(T, SZ) :- shape_props(T, _, _, SZ1), term_string(SZ, SZ1), size(SZ).

exists_shape(SH, [H|_]) :- extract_shape(H, SH).
exists_shape(SH, [_|T]) :- exists_shape(SH, T).

same_shape(SH, [H]) :- extract_shape(H, SH).
same_shape(SH, [H|T]) :- extract_shape(H, SH), same_shape(SH, T).

exists_color(CO, [H|_]) :- extract_color(H, CO).
exists_color(CO, [_|T]) :- exists_color(CO, T).

same_color(CO, [H]) :- extract_color(H, CO).
same_color(CO, [H|T]) :- extract_color(H, CO), same_color(CO, T).

exists_size(SZ, [H|_]) :- extract_size(H, SZ).
exists_size(SZ, [_|T]) :- exists_size(SZ, T).

same_size(SZ, [H]) :- extract_size(H, SZ).
same_size(SZ, [H|T]) :- extract_size(H, SZ), same_size(SZ, T).

contains(C, X) :- defined_as(C, _, L), member(X, L).

recursive_contains(C, X) :- contains(C, X), atomic_obj(X).
recursive_contains(C, X) :- contains(C, C1), recursive_contains(C1, X).


recursive_contains2(C, X, 0) :- contains(C, X), not(atomic_obj(X)).
recursive_contains2(C, X, I) :- contains(C, C1), recursive_contains2(C1, X, J), I is J + 1.

contains_composition(C, COMP) :- defined_as(C, COMP, _).
contains_composition(C, COMP) :- recursive_contains2(C, C1, _), defined_as(C1, COMP, _).

contains_composition_depth(C, COMP, 0) :- defined_as(C, COMP, _).
contains_composition_depth(C, COMP, I) :- recursive_contains2(C, C1, J), defined_as(C1, COMP, _), I is J + 1.


%extract_operator(C, COMP) :- defined_as(C, COMP, _).
%extract_children(C, L) :- defined_as(C, _, L).
%extract_op_and_chld(C, COMP, L) :- defined_as(C, COMP, L).


same_attribute(L) :- same_shape(_, L).
same_attribute(L) :- same_color(_, L).
same_attribute(L) :- same_size(_, L).

same_non_size(L) :- same_shape(_, L).
same_non_size(L) :- same_color(_, L).

all_same_recursion(H, [H]).
all_same_recursion(H, [H|T]) :- all_same_recursion(H, T).

all_same(L) :- all_same_recursion(_, L).


expand2([A, B], A, B).
expand4([A, B, C, D], A, B, C, D).
expand8([A, B, C, D, E, F, G, H], A, B, C, D, E, F, G, H).
expand9([A, B, C, D, E, F, G, H, I], A, B, C, D, E, F, G, H, I).
odd(N) :- N mod 2 =:= 1.
even(N) :- N mod 2 =:= 0.


first([H|_],H).

last([H], H).
last([_|T],X):- last(T, X).


prepend(X, L, [X|L]).
droplast([_], []).
droplast([H|T], [H|T2]):- droplast(T, T2).

middle([_|T], T2):- droplast(T, T2).
getmiddle(L, X) :- length(L, N), odd(N), N1 is div(N, 2), nth0(N1, L, X).
dropmiddle(L, L1) :- getmiddle(L, X), delete(L, X, L1).


less_eq(N, N1) :- N =< N1.
less(N, N1) :- N < N1.
greater(N, N1) :- N > N1.

same_int(X, Y) :- X = Y.
different_int(X, Y) :- X \= Y.

same_obj(X, Y) :- X = Y.
different_obj(X, Y) :- X \= Y.

% USEFUL BUILT-IN PREDICATES:
% atom(X)
% reverse(L1, L2)
% length(L, N)
% delete(L, X, L1)
% nth0(N, L, X)
% member(X, L)

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



atomic_obj(circle_blue_large).
atomic_obj(circle_blue_small).
atomic_obj(circle_cyan_large).
atomic_obj(circle_cyan_small).
atomic_obj(circle_green_large).
atomic_obj(circle_green_small).
atomic_obj(circle_magenta_large).
atomic_obj(circle_magenta_small).
atomic_obj(circle_red_large).
atomic_obj(circle_red_small).
atomic_obj(circle_yellow_large).
atomic_obj(circle_yellow_small).
atomic_obj(square_blue_large).
atomic_obj(square_blue_small).
atomic_obj(square_cyan_large).
atomic_obj(square_cyan_small).
atomic_obj(square_green_large).
atomic_obj(square_green_small).
atomic_obj(square_magenta_large).
atomic_obj(square_magenta_small).
atomic_obj(square_red_large).
atomic_obj(square_red_small).
atomic_obj(square_yellow_large).
atomic_obj(square_yellow_small).
atomic_obj(triangle_blue_large).
atomic_obj(triangle_blue_small).
atomic_obj(triangle_cyan_large).
atomic_obj(triangle_cyan_small).
atomic_obj(triangle_green_large).
atomic_obj(triangle_green_small).
atomic_obj(triangle_magenta_large).
atomic_obj(triangle_magenta_small).
atomic_obj(triangle_red_large).
atomic_obj(triangle_red_small).
atomic_obj(triangle_yellow_large).
atomic_obj(triangle_yellow_small).
defined_as(c000000, grid, [c000001, c000002, c000003, c000004]).
defined_as(c000001, diag_ul_lr, [circle_blue_small, square_blue_large, square_red_large, triangle_cyan_large]).
defined_as(c000002, diag_ul_lr, [square_red_small, circle_magenta_small, triangle_yellow_large, square_magenta_small]).
defined_as(c000003, diag_ul_lr, [square_yellow_small, square_magenta_small, circle_cyan_small, circle_green_small]).
defined_as(c000004, diag_ul_lr, [square_magenta_small, circle_blue_large, triangle_magenta_small, square_blue_large]).
defined_as(c000005, grid, [c000006, c000007, c000008, c000009]).
defined_as(c000006, diag_ul_lr, [square_blue_large, triangle_blue_small, circle_magenta_large, triangle_magenta_small]).
defined_as(c000007, diag_ul_lr, [triangle_green_small, square_cyan_small, triangle_yellow_large, square_red_large]).
defined_as(c000008, diag_ul_lr, [circle_cyan_large, triangle_blue_small, triangle_blue_large, square_cyan_large]).
defined_as(c000009, diag_ul_lr, [circle_cyan_large, triangle_magenta_large, triangle_blue_large, circle_green_small]).
defined_as(c000010, grid, [c000011, c000012, c000013, c000014]).
defined_as(c000011, diag_ul_lr, [triangle_red_large, triangle_red_small, triangle_red_small, triangle_red_small]).
defined_as(c000012, diag_ul_lr, [triangle_magenta_small, triangle_red_large, square_yellow_small, triangle_cyan_large]).
defined_as(c000013, diag_ul_lr, [triangle_red_large, triangle_blue_small, square_yellow_small, circle_magenta_small]).
defined_as(c000014, diag_ul_lr, [triangle_red_large, triangle_cyan_large, circle_red_large, circle_green_large]).
defined_as(c000015, grid, [c000016, c000017, c000018, c000019]).
defined_as(c000016, diag_ul_lr, [circle_cyan_large, triangle_cyan_small, triangle_red_small, circle_green_large]).
defined_as(c000017, diag_ul_lr, [square_cyan_large, circle_green_small, triangle_yellow_large, square_green_large]).
defined_as(c000018, diag_ul_lr, [triangle_yellow_small, triangle_yellow_small, circle_red_small, circle_cyan_small]).
defined_as(c000019, diag_ul_lr, [circle_cyan_small, square_red_small, triangle_yellow_large, circle_blue_large]).
defined_as(c000020, grid, [c000021, c000022, c000023, c000024]).
defined_as(c000021, diag_ul_lr, [circle_yellow_large, circle_cyan_small, square_yellow_small, square_blue_large]).
defined_as(c000022, diag_ul_lr, [square_cyan_small, circle_magenta_large, square_blue_large, triangle_yellow_large]).
defined_as(c000023, diag_ul_lr, [triangle_yellow_large, square_cyan_large, circle_blue_large, triangle_green_small]).
defined_as(c000024, diag_ul_lr, [triangle_cyan_large, triangle_magenta_large, circle_cyan_small, triangle_magenta_small]).
defined_as(c000025, grid, [c000026, c000027, c000028, c000029]).
defined_as(c000026, diag_ul_lr, [triangle_blue_small, square_magenta_small, square_magenta_small, circle_cyan_large]).
defined_as(c000027, diag_ul_lr, [triangle_red_large, square_cyan_small, circle_green_large, square_yellow_large]).
defined_as(c000028, diag_ul_lr, [triangle_yellow_large, triangle_red_small, triangle_cyan_small, square_blue_small]).
defined_as(c000029, diag_ul_lr, [circle_green_large, triangle_red_large, circle_magenta_large, circle_red_small]).
defined_as(c000030, grid, [c000031, c000032, c000033, c000034]).
defined_as(c000031, diag_ul_lr, [square_yellow_large, circle_red_large, circle_yellow_large, circle_red_large]).
defined_as(c000032, diag_ul_lr, [triangle_yellow_small, triangle_red_small, triangle_magenta_large, triangle_cyan_large]).
defined_as(c000033, diag_ul_lr, [square_yellow_large, circle_blue_large, triangle_red_small, square_green_small]).
defined_as(c000034, diag_ul_lr, [circle_magenta_small, square_red_small, square_magenta_small, triangle_magenta_large]).
defined_as(c000035, grid, [c000036, c000037, c000038, c000039]).
defined_as(c000036, diag_ul_lr, [triangle_blue_small, triangle_cyan_small, triangle_green_large, square_red_large]).
defined_as(c000037, diag_ul_lr, [square_yellow_large, circle_cyan_small, triangle_magenta_small, square_magenta_small]).
defined_as(c000038, diag_ul_lr, [square_cyan_large, square_magenta_small, square_blue_small, circle_yellow_small]).
defined_as(c000039, diag_ul_lr, [triangle_yellow_small, square_red_large, square_green_large, triangle_red_small]).
defined_as(c000040, grid, [c000041, c000042, c000043, c000044]).
defined_as(c000041, diag_ul_lr, [square_red_small, triangle_red_large, circle_blue_large, circle_magenta_large]).
defined_as(c000042, diag_ul_lr, [square_magenta_large, triangle_red_small, circle_blue_large, square_green_large]).
defined_as(c000043, diag_ul_lr, [square_cyan_large, triangle_red_large, square_magenta_large, circle_yellow_large]).
defined_as(c000044, diag_ul_lr, [square_green_small, triangle_magenta_small, circle_magenta_small, triangle_red_small]).
defined_as(c000045, grid, [c000046, c000047, c000048, c000049]).
defined_as(c000046, diag_ul_lr, [triangle_yellow_large, circle_cyan_small, square_yellow_large, triangle_green_small]).
defined_as(c000047, diag_ul_lr, [circle_green_small, square_green_small, square_green_small, triangle_blue_large]).
defined_as(c000048, diag_ul_lr, [triangle_cyan_large, circle_blue_small, square_cyan_small, triangle_blue_large]).
defined_as(c000049, diag_ul_lr, [circle_magenta_small, square_red_small, triangle_red_large, triangle_yellow_large]).
defined_as(c000050, grid, [c000051, c000052, c000053, c000054]).
defined_as(c000051, diag_ul_lr, [circle_green_large, triangle_green_small, triangle_red_large, triangle_red_small]).
defined_as(c000052, diag_ul_lr, [triangle_red_small, square_red_small, square_green_large, triangle_red_small]).
defined_as(c000053, diag_ul_lr, [circle_cyan_large, triangle_red_large, circle_green_large, square_cyan_large]).
defined_as(c000054, diag_ul_lr, [triangle_cyan_large, circle_cyan_large, triangle_blue_large, triangle_red_large]).
defined_as(c000055, grid, [c000056, c000057, c000058, c000059]).
defined_as(c000056, diag_ul_lr, [circle_blue_small, circle_green_small, triangle_blue_small, circle_yellow_large]).
defined_as(c000057, diag_ul_lr, [triangle_yellow_small, square_yellow_small, triangle_blue_large, triangle_blue_large]).
defined_as(c000058, diag_ul_lr, [square_yellow_large, triangle_cyan_large, square_cyan_large, circle_yellow_small]).
defined_as(c000059, diag_ul_lr, [circle_yellow_small, circle_green_small, square_red_small, circle_yellow_large]).
defined_as(c000060, grid, [c000061, c000062, c000063, c000064]).
defined_as(c000061, diag_ul_lr, [circle_yellow_large, circle_green_small, circle_blue_small, triangle_cyan_large]).
defined_as(c000062, diag_ul_lr, [square_red_large, square_green_small, square_blue_large, square_yellow_large]).
defined_as(c000063, diag_ul_lr, [triangle_cyan_large, square_blue_small, circle_magenta_small, square_red_small]).
defined_as(c000064, diag_ul_lr, [square_magenta_large, triangle_blue_small, circle_magenta_small, triangle_cyan_small]).
defined_as(c000065, grid, [c000066, c000067, c000068, c000069]).
defined_as(c000066, diag_ul_lr, [circle_magenta_small, triangle_yellow_small, triangle_yellow_large, square_cyan_large]).
defined_as(c000067, diag_ul_lr, [circle_magenta_small, square_yellow_small, triangle_cyan_small, circle_green_small]).
defined_as(c000068, diag_ul_lr, [square_red_large, square_yellow_small, circle_yellow_large, triangle_blue_small]).
defined_as(c000069, diag_ul_lr, [square_green_small, square_blue_large, square_blue_small, square_cyan_large]).
defined_as(c000070, grid, [c000071, c000072, c000073, c000074]).
defined_as(c000071, diag_ul_lr, [triangle_green_large, triangle_red_large, triangle_blue_large, square_green_small]).
defined_as(c000072, diag_ul_lr, [square_blue_large, triangle_red_small, triangle_magenta_large, square_cyan_large]).
defined_as(c000073, diag_ul_lr, [circle_magenta_large, triangle_cyan_large, triangle_red_large, triangle_green_small]).
defined_as(c000074, diag_ul_lr, [circle_cyan_large, triangle_red_small, triangle_yellow_large, triangle_cyan_large]).
defined_as(c000075, grid, [c000076, c000077, c000078, c000079]).
defined_as(c000076, diag_ul_lr, [square_green_large, square_green_small, triangle_cyan_small, triangle_red_small]).
defined_as(c000077, diag_ul_lr, [triangle_red_small, square_red_small, square_red_large, square_red_small]).
defined_as(c000078, diag_ul_lr, [triangle_blue_large, square_green_small, circle_blue_large, triangle_red_small]).
defined_as(c000079, diag_ul_lr, [square_magenta_small, circle_magenta_small, triangle_red_small, circle_magenta_large]).
defined_as(c000080, grid, [c000081, c000082, c000083, c000084]).
defined_as(c000081, diag_ul_lr, [triangle_red_small, circle_blue_small, triangle_cyan_large, triangle_red_small]).
defined_as(c000082, diag_ul_lr, [square_red_small, triangle_red_small, square_green_small, square_magenta_small]).
defined_as(c000083, diag_ul_lr, [triangle_red_small, circle_green_small, square_red_large, triangle_red_large]).
defined_as(c000084, diag_ul_lr, [circle_magenta_small, triangle_red_small, circle_yellow_large, circle_blue_small]).
defined_as(c000085, grid, [c000086, c000087, c000088, c000089]).
defined_as(c000086, diag_ul_lr, [square_cyan_large, triangle_yellow_small, triangle_magenta_large, square_green_small]).
defined_as(c000087, diag_ul_lr, [square_yellow_large, square_yellow_large, square_blue_small, square_yellow_small]).
defined_as(c000088, diag_ul_lr, [triangle_cyan_large, circle_cyan_large, square_red_small, triangle_cyan_small]).
defined_as(c000089, diag_ul_lr, [square_magenta_large, square_green_small, square_yellow_small, circle_red_small]).
defined_as(c000090, grid, [c000091, c000092, c000093, c000094]).
defined_as(c000091, diag_ul_lr, [circle_yellow_small, circle_yellow_small, triangle_red_small, square_red_large]).
defined_as(c000092, diag_ul_lr, [circle_cyan_small, square_yellow_small, circle_red_small, triangle_red_small]).
defined_as(c000093, diag_ul_lr, [triangle_red_large, circle_cyan_large, square_red_small, circle_cyan_large]).
defined_as(c000094, diag_ul_lr, [triangle_blue_large, circle_cyan_small, square_magenta_small, triangle_red_small]).
defined_as(c000095, grid, [c000096, c000097, c000098, c000099]).
defined_as(c000096, diag_ul_lr, [triangle_yellow_small, square_yellow_large, circle_yellow_large, circle_yellow_large]).
defined_as(c000097, diag_ul_lr, [circle_magenta_large, square_yellow_large, triangle_blue_large, triangle_blue_small]).
defined_as(c000098, diag_ul_lr, [square_yellow_large, square_blue_small, circle_red_small, triangle_magenta_large]).
defined_as(c000099, diag_ul_lr, [circle_magenta_large, square_cyan_large, triangle_red_large, triangle_red_large]).
defined_as(c000100, grid, [c000101, c000102, c000103, c000104]).
defined_as(c000101, diag_ul_lr, [triangle_blue_large, triangle_red_large, square_magenta_large, square_blue_small]).
defined_as(c000102, diag_ul_lr, [triangle_red_large, triangle_green_large, triangle_red_large, square_magenta_large]).
defined_as(c000103, diag_ul_lr, [triangle_red_large, square_yellow_small, square_green_small, square_red_small]).
defined_as(c000104, diag_ul_lr, [circle_magenta_large, circle_red_large, triangle_cyan_large, triangle_red_large]).
defined_as(c000105, grid, [c000106, c000107, c000108, c000109]).
defined_as(c000106, diag_ul_lr, [square_magenta_large, triangle_cyan_small, circle_blue_small, circle_red_large]).
defined_as(c000107, diag_ul_lr, [square_cyan_large, triangle_cyan_small, circle_red_large, triangle_blue_small]).
defined_as(c000108, diag_ul_lr, [circle_yellow_small, square_cyan_large, triangle_magenta_small, square_magenta_large]).
defined_as(c000109, diag_ul_lr, [circle_magenta_small, square_magenta_small, triangle_blue_small, circle_yellow_large]).
defined_as(c000110, grid, [c000111, c000112, c000113, c000114]).
defined_as(c000111, diag_ul_lr, [square_blue_large, triangle_red_small, triangle_red_large, circle_red_large]).
defined_as(c000112, diag_ul_lr, [square_yellow_large, circle_red_large, triangle_red_small, triangle_cyan_small]).
defined_as(c000113, diag_ul_lr, [triangle_yellow_large, triangle_cyan_small, square_magenta_small, triangle_red_small]).
defined_as(c000114, diag_ul_lr, [square_blue_small, triangle_red_large, square_cyan_large, circle_green_small]).
defined_as(c000115, grid, [c000116, c000117, c000118, c000119]).
defined_as(c000116, diag_ul_lr, [circle_cyan_small, triangle_red_small, triangle_red_small, circle_green_large]).
defined_as(c000117, diag_ul_lr, [triangle_red_large, square_cyan_small, circle_blue_small, circle_green_large]).
defined_as(c000118, diag_ul_lr, [circle_red_small, triangle_red_large, triangle_green_small, circle_blue_small]).
defined_as(c000119, diag_ul_lr, [square_magenta_small, triangle_red_large, triangle_yellow_large, circle_cyan_small]).
defined_as(c000120, grid, [c000121, c000122, c000123, c000124]).
defined_as(c000121, diag_ul_lr, [triangle_red_small, square_cyan_large, triangle_magenta_large, square_blue_small]).
defined_as(c000122, diag_ul_lr, [circle_cyan_large, triangle_red_large, circle_cyan_small, circle_yellow_small]).
defined_as(c000123, diag_ul_lr, [square_magenta_small, triangle_red_small, circle_magenta_small, triangle_yellow_large]).
defined_as(c000124, diag_ul_lr, [square_blue_large, circle_green_small, triangle_red_small, triangle_red_small]).
defined_as(c000125, grid, [c000126, c000127, c000128, c000129]).
defined_as(c000126, diag_ul_lr, [triangle_red_small, circle_green_small, square_yellow_small, triangle_red_small]).
defined_as(c000127, diag_ul_lr, [triangle_magenta_small, triangle_green_large, triangle_red_large, circle_red_small]).
defined_as(c000128, diag_ul_lr, [triangle_red_small, square_green_small, triangle_red_large, square_green_large]).
defined_as(c000129, diag_ul_lr, [triangle_red_small, square_green_small, square_red_small, triangle_blue_large]).
defined_as(c000130, grid, [c000131, c000132, c000133, c000134]).
defined_as(c000131, diag_ul_lr, [triangle_red_small, circle_red_large, circle_magenta_large, triangle_blue_large]).
defined_as(c000132, diag_ul_lr, [square_green_small, triangle_red_large, triangle_red_small, square_red_small]).
defined_as(c000133, diag_ul_lr, [square_green_large, square_red_large, triangle_red_large, square_blue_small]).
defined_as(c000134, diag_ul_lr, [square_magenta_small, square_red_large, triangle_red_large, square_magenta_small]).
defined_as(c000135, grid, [c000136, c000137, c000138, c000139]).
defined_as(c000136, diag_ul_lr, [triangle_red_large, square_magenta_small, triangle_blue_large, square_magenta_large]).
defined_as(c000137, diag_ul_lr, [circle_red_small, circle_red_large, triangle_red_small, triangle_red_large]).
defined_as(c000138, diag_ul_lr, [triangle_red_small, square_yellow_small, square_yellow_small, triangle_blue_small]).
defined_as(c000139, diag_ul_lr, [triangle_magenta_large, square_cyan_large, square_yellow_small, triangle_red_large]).
defined_as(c000140, grid, [c000141, c000142, c000143, c000144]).
defined_as(c000141, diag_ul_lr, [square_red_small, triangle_green_small, square_yellow_large, circle_blue_large]).
defined_as(c000142, diag_ul_lr, [circle_red_large, square_cyan_small, circle_red_large, circle_red_small]).
defined_as(c000143, diag_ul_lr, [triangle_magenta_large, square_green_small, circle_cyan_large, square_cyan_small]).
defined_as(c000144, diag_ul_lr, [circle_green_large, square_red_large, triangle_magenta_large, circle_red_small]).
defined_as(c000145, grid, [c000146, c000147, c000148, c000149]).
defined_as(c000146, diag_ul_lr, [square_red_large, square_magenta_small, triangle_green_small, triangle_red_large]).
defined_as(c000147, diag_ul_lr, [circle_green_small, circle_yellow_large, triangle_green_small, triangle_red_large]).
defined_as(c000148, diag_ul_lr, [square_red_small, triangle_yellow_large, triangle_red_small, triangle_red_small]).
defined_as(c000149, diag_ul_lr, [circle_cyan_large, circle_yellow_large, square_green_small, triangle_red_large]).
defined_as(c000150, grid, [c000151, c000152, c000153, c000154]).
defined_as(c000151, diag_ul_lr, [circle_red_small, circle_green_large, circle_green_large, circle_magenta_large]).
defined_as(c000152, diag_ul_lr, [circle_red_small, triangle_yellow_small, circle_red_small, triangle_cyan_small]).
defined_as(c000153, diag_ul_lr, [square_green_small, square_magenta_small, square_yellow_large, triangle_green_large]).
defined_as(c000154, diag_ul_lr, [circle_blue_large, square_green_small, square_magenta_small, circle_blue_small]).
defined_as(c000155, grid, [c000156, c000157, c000158, c000159]).
defined_as(c000156, diag_ul_lr, [circle_yellow_large, circle_magenta_large, square_cyan_small, circle_yellow_small]).
defined_as(c000157, diag_ul_lr, [square_green_small, triangle_green_large, circle_magenta_large, triangle_magenta_small]).
defined_as(c000158, diag_ul_lr, [triangle_cyan_small, square_red_small, circle_blue_small, triangle_green_large]).
defined_as(c000159, diag_ul_lr, [square_blue_small, circle_green_small, circle_cyan_small, square_yellow_large]).
defined_as(c000160, grid, [c000161, c000162, c000163, c000164]).
defined_as(c000161, diag_ul_lr, [triangle_green_large, triangle_red_large, square_blue_large, circle_yellow_small]).
defined_as(c000162, diag_ul_lr, [circle_blue_small, triangle_red_large, circle_cyan_small, triangle_magenta_small]).
defined_as(c000163, diag_ul_lr, [triangle_red_large, circle_magenta_large, square_green_small, triangle_green_large]).
defined_as(c000164, diag_ul_lr, [square_blue_large, triangle_red_small, square_green_large, triangle_cyan_small]).
defined_as(c000165, grid, [c000166, c000167, c000168, c000169]).
defined_as(c000166, diag_ul_lr, [triangle_red_large, circle_magenta_small, square_cyan_large, square_red_small]).
defined_as(c000167, diag_ul_lr, [square_cyan_large, square_magenta_large, circle_cyan_large, square_magenta_small]).
defined_as(c000168, diag_ul_lr, [triangle_blue_large, triangle_blue_small, square_magenta_large, circle_yellow_small]).
defined_as(c000169, diag_ul_lr, [triangle_yellow_large, square_blue_large, square_green_small, triangle_yellow_large]).
defined_as(c000170, grid, [c000171, c000172, c000173, c000174]).
defined_as(c000171, diag_ul_lr, [square_yellow_large, circle_green_small, triangle_red_small, square_yellow_small]).
defined_as(c000172, diag_ul_lr, [triangle_red_small, circle_magenta_large, triangle_green_small, triangle_magenta_large]).
defined_as(c000173, diag_ul_lr, [triangle_red_small, triangle_red_small, square_cyan_large, triangle_blue_large]).
defined_as(c000174, diag_ul_lr, [triangle_yellow_small, triangle_red_large, triangle_red_small, circle_red_small]).
defined_as(c000175, grid, [c000176, c000177, c000178, c000179]).
defined_as(c000176, diag_ul_lr, [circle_blue_small, triangle_red_small, square_green_small, square_yellow_large]).
defined_as(c000177, diag_ul_lr, [circle_magenta_small, triangle_cyan_small, circle_magenta_large, triangle_red_large]).
defined_as(c000178, diag_ul_lr, [triangle_red_small, triangle_yellow_large, circle_cyan_large, triangle_red_small]).
defined_as(c000179, diag_ul_lr, [triangle_red_large, square_cyan_small, circle_red_small, circle_red_small]).
defined_as(c000180, grid, [c000181, c000182, c000183, c000184]).
defined_as(c000181, diag_ul_lr, [square_red_large, triangle_blue_large, circle_green_large, circle_cyan_large]).
defined_as(c000182, diag_ul_lr, [triangle_cyan_large, square_blue_small, circle_yellow_large, triangle_green_large]).
defined_as(c000183, diag_ul_lr, [circle_magenta_small, square_red_small, circle_red_large, triangle_red_large]).
defined_as(c000184, diag_ul_lr, [square_yellow_large, square_cyan_large, circle_green_large, circle_magenta_large]).
defined_as(c000185, grid, [c000186, c000187, c000188, c000189]).
defined_as(c000186, diag_ul_lr, [triangle_cyan_large, circle_blue_small, square_red_small, circle_green_large]).
defined_as(c000187, diag_ul_lr, [square_cyan_small, square_yellow_small, square_red_small, circle_blue_small]).
defined_as(c000188, diag_ul_lr, [square_red_small, square_green_small, circle_green_large, triangle_yellow_large]).
defined_as(c000189, diag_ul_lr, [circle_yellow_large, square_blue_small, square_red_large, square_green_large]).
defined_as(c000190, grid, [c000191, c000192, c000193, c000194]).
defined_as(c000191, diag_ul_lr, [circle_cyan_large, triangle_blue_large, triangle_magenta_large, triangle_red_large]).
defined_as(c000192, diag_ul_lr, [square_cyan_large, triangle_cyan_large, triangle_yellow_small, triangle_red_large]).
defined_as(c000193, diag_ul_lr, [square_yellow_large, triangle_yellow_large, triangle_cyan_small, triangle_red_small]).
defined_as(c000194, diag_ul_lr, [triangle_yellow_small, square_green_small, square_green_large, triangle_red_small]).
defined_as(c000195, grid, [c000196, c000197, c000198, c000199]).
defined_as(c000196, diag_ul_lr, [circle_green_small, triangle_cyan_large, circle_magenta_large, triangle_blue_large]).
defined_as(c000197, diag_ul_lr, [square_red_small, circle_green_large, circle_green_large, circle_blue_large]).
defined_as(c000198, diag_ul_lr, [circle_green_large, square_green_small, square_blue_large, square_yellow_large]).
defined_as(c000199, diag_ul_lr, [circle_yellow_large, triangle_red_large, triangle_green_small, triangle_red_small]).
defined_as(c000200, grid, [c000201, c000202, c000203, c000204]).
defined_as(c000201, diag_ul_lr, [triangle_red_large, circle_yellow_small, triangle_cyan_small, triangle_blue_large]).
defined_as(c000202, diag_ul_lr, [square_green_small, triangle_red_small, square_cyan_large, circle_yellow_large]).
defined_as(c000203, diag_ul_lr, [triangle_red_large, square_yellow_large, triangle_green_large, square_yellow_small]).
defined_as(c000204, diag_ul_lr, [triangle_red_large, circle_yellow_large, triangle_magenta_large, square_green_small]).
defined_as(c000205, grid, [c000206, c000207, c000208, c000209]).
defined_as(c000206, diag_ul_lr, [triangle_red_small, square_red_large, triangle_blue_small, circle_yellow_small]).
defined_as(c000207, diag_ul_lr, [square_red_large, triangle_red_large, square_red_small, circle_yellow_small]).
defined_as(c000208, diag_ul_lr, [triangle_cyan_small, circle_magenta_large, triangle_red_large, triangle_green_large]).
defined_as(c000209, diag_ul_lr, [triangle_cyan_large, square_magenta_small, triangle_red_large, triangle_red_large]).
defined_as(c000210, grid, [c000211, c000212, c000213, c000214]).
defined_as(c000211, diag_ul_lr, [triangle_red_large, circle_blue_small, circle_cyan_small, triangle_cyan_large]).
defined_as(c000212, diag_ul_lr, [square_blue_small, triangle_red_large, circle_cyan_small, circle_red_large]).
defined_as(c000213, diag_ul_lr, [triangle_red_small, square_red_large, triangle_cyan_small, circle_blue_large]).
defined_as(c000214, diag_ul_lr, [triangle_red_small, triangle_yellow_small, square_yellow_small, square_cyan_small]).
defined_as(c000215, grid, [c000216, c000217, c000218, c000219]).
defined_as(c000216, diag_ul_lr, [triangle_cyan_small, triangle_magenta_large, triangle_green_small, square_yellow_large]).
defined_as(c000217, diag_ul_lr, [triangle_magenta_small, square_blue_small, square_blue_small, triangle_yellow_large]).
defined_as(c000218, diag_ul_lr, [triangle_blue_large, square_red_large, circle_yellow_small, square_cyan_large]).
defined_as(c000219, diag_ul_lr, [circle_cyan_small, triangle_cyan_large, circle_green_large, square_yellow_large]).
defined_as(c000220, grid, [c000221, c000222, c000223, c000224]).
defined_as(c000221, diag_ul_lr, [circle_yellow_large, triangle_red_small, square_red_small, circle_green_large]).
defined_as(c000222, diag_ul_lr, [circle_magenta_small, circle_magenta_small, triangle_red_small, triangle_cyan_large]).
defined_as(c000223, diag_ul_lr, [triangle_green_small, circle_yellow_large, triangle_red_large, triangle_magenta_small]).
defined_as(c000224, diag_ul_lr, [square_magenta_large, triangle_magenta_large, triangle_red_small, square_cyan_large]).
defined_as(c000225, grid, [c000226, c000227, c000228, c000229]).
defined_as(c000226, diag_ul_lr, [circle_yellow_large, triangle_cyan_large, triangle_blue_large, square_cyan_small]).
defined_as(c000227, diag_ul_lr, [triangle_blue_small, square_red_large, triangle_green_large, square_yellow_large]).
defined_as(c000228, diag_ul_lr, [circle_yellow_large, square_magenta_large, circle_red_large, triangle_red_small]).
defined_as(c000229, diag_ul_lr, [circle_yellow_small, square_cyan_large, triangle_green_large, square_green_small]).
defined_as(c000230, grid, [c000231, c000232, c000233, c000234]).
defined_as(c000231, diag_ul_lr, [circle_yellow_small, square_green_small, triangle_red_large, square_magenta_small]).
defined_as(c000232, diag_ul_lr, [circle_yellow_small, triangle_green_large, circle_green_small, square_cyan_small]).
defined_as(c000233, diag_ul_lr, [triangle_yellow_small, triangle_red_small, square_cyan_large, triangle_blue_small]).
defined_as(c000234, diag_ul_lr, [square_blue_small, square_magenta_small, circle_magenta_small, triangle_yellow_small]).
defined_as(c000235, grid, [c000236, c000237, c000238, c000239]).
defined_as(c000236, diag_ul_lr, [square_yellow_small, triangle_cyan_small, triangle_red_small, square_red_small]).
defined_as(c000237, diag_ul_lr, [circle_blue_small, circle_cyan_large, triangle_blue_small, triangle_red_large]).
defined_as(c000238, diag_ul_lr, [triangle_red_large, square_green_large, square_green_large, circle_cyan_large]).
defined_as(c000239, diag_ul_lr, [triangle_red_small, circle_magenta_small, circle_magenta_large, triangle_cyan_large]).
defined_as(c000240, grid, [c000241, c000242, c000243, c000244]).
defined_as(c000241, diag_ul_lr, [triangle_blue_large, triangle_red_small, circle_cyan_small, circle_cyan_large]).
defined_as(c000242, diag_ul_lr, [triangle_cyan_small, circle_yellow_large, triangle_magenta_small, triangle_red_large]).
defined_as(c000243, diag_ul_lr, [square_red_small, triangle_red_large, circle_red_large, square_red_small]).
defined_as(c000244, diag_ul_lr, [circle_blue_large, circle_yellow_small, triangle_red_large, triangle_red_large]).
defined_as(c000245, grid, [c000246, c000247, c000248, c000249]).
defined_as(c000246, diag_ul_lr, [triangle_green_small, square_red_small, square_blue_small, square_magenta_large]).
defined_as(c000247, diag_ul_lr, [square_green_small, triangle_blue_large, circle_cyan_large, square_cyan_small]).
defined_as(c000248, diag_ul_lr, [circle_green_large, triangle_yellow_small, circle_magenta_small, square_green_large]).
defined_as(c000249, diag_ul_lr, [triangle_cyan_small, square_cyan_large, circle_red_large, triangle_yellow_large]).
defined_as(c000250, grid, [c000251, c000252, c000253, c000254]).
defined_as(c000251, diag_ul_lr, [triangle_red_small, square_red_large, square_green_small, triangle_yellow_large]).
defined_as(c000252, diag_ul_lr, [square_red_large, triangle_yellow_small, square_green_small, square_green_small]).
defined_as(c000253, diag_ul_lr, [square_red_small, square_red_small, triangle_green_small, circle_red_large]).
defined_as(c000254, diag_ul_lr, [triangle_blue_small, triangle_magenta_small, circle_red_small, triangle_red_small]).
defined_as(c000255, grid, [c000256, c000257, c000258, c000259]).
defined_as(c000256, diag_ul_lr, [triangle_blue_large, circle_yellow_small, circle_magenta_small, triangle_red_large]).
defined_as(c000257, diag_ul_lr, [circle_green_large, triangle_red_large, triangle_red_small, circle_blue_large]).
defined_as(c000258, diag_ul_lr, [square_red_large, square_blue_small, triangle_red_large, circle_green_small]).
defined_as(c000259, diag_ul_lr, [square_cyan_large, square_yellow_large, triangle_red_large, triangle_cyan_small]).
defined_as(c000260, grid, [c000261, c000262, c000263, c000264]).
defined_as(c000261, diag_ul_lr, [square_blue_large, square_green_small, triangle_red_small, circle_magenta_small]).
defined_as(c000262, diag_ul_lr, [circle_red_small, triangle_red_large, square_cyan_large, triangle_red_small]).
defined_as(c000263, diag_ul_lr, [square_green_large, circle_cyan_large, square_blue_small, triangle_red_large]).
defined_as(c000264, diag_ul_lr, [triangle_cyan_small, triangle_red_small, square_red_large, square_red_large]).
defined_as(c000265, grid, [c000266, c000267, c000268, c000269]).
defined_as(c000266, diag_ul_lr, [triangle_green_large, square_blue_large, square_cyan_large, circle_magenta_large]).
defined_as(c000267, diag_ul_lr, [circle_red_small, square_green_small, square_magenta_small, square_cyan_large]).
defined_as(c000268, diag_ul_lr, [square_green_large, circle_yellow_large, square_cyan_small, square_magenta_small]).
defined_as(c000269, diag_ul_lr, [square_magenta_large, square_yellow_large, circle_yellow_small, circle_green_small]).
defined_as(c000270, grid, [c000271, c000272, c000273, c000274]).
defined_as(c000271, diag_ul_lr, [triangle_yellow_small, square_magenta_large, circle_red_large, circle_green_large]).
defined_as(c000272, diag_ul_lr, [square_red_large, circle_yellow_small, square_cyan_large, triangle_green_small]).
defined_as(c000273, diag_ul_lr, [circle_cyan_large, circle_yellow_small, triangle_blue_large, triangle_yellow_small]).
defined_as(c000274, diag_ul_lr, [triangle_yellow_large, triangle_cyan_small, triangle_yellow_small, circle_green_small]).
defined_as(c000275, grid, [c000276, c000277, c000278, c000279]).
defined_as(c000276, diag_ul_lr, [triangle_yellow_large, square_magenta_small, square_blue_large, circle_green_large]).
defined_as(c000277, diag_ul_lr, [triangle_green_small, square_magenta_small, triangle_blue_small, triangle_magenta_large]).
defined_as(c000278, diag_ul_lr, [square_magenta_large, square_magenta_large, square_blue_small, triangle_green_large]).
defined_as(c000279, diag_ul_lr, [square_blue_small, triangle_cyan_large, triangle_blue_small, circle_green_small]).
defined_as(c000280, grid, [c000281, c000282, c000283, c000284]).
defined_as(c000281, diag_ul_lr, [triangle_yellow_small, triangle_red_large, circle_yellow_large, square_yellow_small]).
defined_as(c000282, diag_ul_lr, [circle_blue_large, square_magenta_small, circle_red_small, triangle_red_large]).
defined_as(c000283, diag_ul_lr, [square_blue_small, triangle_red_small, circle_magenta_small, square_red_large]).
defined_as(c000284, diag_ul_lr, [square_red_small, triangle_cyan_small, triangle_red_large, circle_green_small]).
defined_as(c000285, grid, [c000286, c000287, c000288, c000289]).
defined_as(c000286, diag_ul_lr, [triangle_blue_large, triangle_red_large, square_cyan_large, triangle_red_small]).
defined_as(c000287, diag_ul_lr, [triangle_magenta_large, circle_red_small, triangle_red_small, triangle_red_large]).
defined_as(c000288, diag_ul_lr, [circle_red_large, square_blue_small, square_cyan_large, triangle_red_large]).
defined_as(c000289, diag_ul_lr, [triangle_magenta_large, triangle_red_small, circle_yellow_small, triangle_red_small]).
defined_as(c000290, grid, [c000291, c000292, c000293, c000294]).
defined_as(c000291, diag_ul_lr, [triangle_blue_small, circle_red_large, triangle_magenta_small, circle_cyan_large]).
defined_as(c000292, diag_ul_lr, [triangle_magenta_small, square_cyan_large, square_magenta_large, square_red_large]).
defined_as(c000293, diag_ul_lr, [circle_green_small, circle_green_small, square_yellow_large, triangle_magenta_small]).
defined_as(c000294, diag_ul_lr, [triangle_green_large, triangle_cyan_large, circle_green_small, square_blue_small]).
defined_as(c000295, grid, [c000296, c000297, c000298, c000299]).
defined_as(c000296, diag_ul_lr, [triangle_magenta_large, square_magenta_small, triangle_green_small, square_red_large]).
defined_as(c000297, diag_ul_lr, [square_blue_large, circle_magenta_small, square_blue_small, circle_green_large]).
defined_as(c000298, diag_ul_lr, [circle_cyan_small, circle_green_small, circle_red_large, square_blue_small]).
defined_as(c000299, diag_ul_lr, [square_cyan_small, circle_cyan_small, triangle_blue_large, circle_cyan_large]).
defined_as(c000300, grid, [c000301, c000302, c000303, c000304]).
defined_as(c000301, diag_ul_lr, [triangle_red_small, square_blue_large, triangle_green_small, triangle_cyan_large]).
defined_as(c000302, diag_ul_lr, [circle_blue_small, circle_red_small, circle_green_small, circle_yellow_small]).
defined_as(c000303, diag_ul_lr, [square_cyan_small, triangle_blue_small, square_red_small, circle_green_large]).
defined_as(c000304, diag_ul_lr, [triangle_green_small, triangle_cyan_small, circle_magenta_small, square_green_large]).
defined_as(c000305, grid, [c000306, c000307, c000308, c000309]).
defined_as(c000306, diag_ul_lr, [circle_blue_small, circle_blue_large, triangle_blue_large, circle_blue_small]).
defined_as(c000307, diag_ul_lr, [circle_blue_large, square_cyan_large, triangle_blue_large, triangle_yellow_large]).
defined_as(c000308, diag_ul_lr, [triangle_cyan_large, square_yellow_small, square_green_small, square_yellow_large]).
defined_as(c000309, diag_ul_lr, [circle_green_large, circle_green_small, circle_yellow_small, triangle_cyan_small]).
defined_as(c000310, grid, [c000311, c000312, c000313, c000314]).
defined_as(c000311, diag_ul_lr, [circle_yellow_small, circle_magenta_large, square_blue_small, square_green_small]).
defined_as(c000312, diag_ul_lr, [circle_red_large, circle_magenta_large, triangle_green_small, triangle_green_small]).
defined_as(c000313, diag_ul_lr, [square_yellow_small, triangle_cyan_small, triangle_yellow_large, triangle_cyan_large]).
defined_as(c000314, diag_ul_lr, [triangle_red_large, circle_blue_small, circle_cyan_large, circle_blue_small]).
defined_as(c000315, grid, [c000316, c000317, c000318, c000319]).
defined_as(c000316, diag_ul_lr, [circle_blue_small, circle_cyan_large, square_green_small, triangle_red_large]).
defined_as(c000317, diag_ul_lr, [triangle_red_small, triangle_red_small, circle_green_large, triangle_blue_small]).
defined_as(c000318, diag_ul_lr, [circle_magenta_large, triangle_green_large, triangle_red_small, circle_yellow_small]).
defined_as(c000319, diag_ul_lr, [triangle_green_large, square_magenta_large, triangle_magenta_small, triangle_red_small]).
defined_as(c000320, grid, [c000321, c000322, c000323, c000324]).
defined_as(c000321, diag_ul_lr, [square_green_large, triangle_red_large, square_yellow_small, triangle_green_large]).
defined_as(c000322, diag_ul_lr, [triangle_red_large, circle_red_small, circle_red_small, square_cyan_large]).
defined_as(c000323, diag_ul_lr, [circle_magenta_small, circle_green_small, triangle_red_large, square_cyan_large]).
defined_as(c000324, diag_ul_lr, [circle_red_small, triangle_magenta_small, square_red_small, triangle_red_large]).
defined_as(c000325, grid, [c000326, c000327, c000328, c000329]).
defined_as(c000326, diag_ul_lr, [square_yellow_small, circle_green_large, square_magenta_small, triangle_red_small]).
defined_as(c000327, diag_ul_lr, [circle_magenta_small, triangle_yellow_small, triangle_magenta_small, circle_blue_small]).
defined_as(c000328, diag_ul_lr, [circle_green_large, triangle_blue_large, triangle_yellow_large, square_yellow_small]).
defined_as(c000329, diag_ul_lr, [circle_magenta_small, circle_cyan_large, square_red_large, circle_green_large]).
defined_as(c000330, grid, [c000331, c000332, c000333, c000334]).
defined_as(c000331, diag_ul_lr, [triangle_red_small, triangle_magenta_small, triangle_cyan_large, triangle_blue_small]).
defined_as(c000332, diag_ul_lr, [circle_blue_small, square_magenta_large, circle_cyan_large, square_green_large]).
defined_as(c000333, diag_ul_lr, [circle_yellow_large, circle_blue_large, triangle_blue_large, square_red_large]).
defined_as(c000334, diag_ul_lr, [triangle_cyan_large, square_yellow_small, triangle_cyan_large, circle_green_small]).
defined_as(c000335, grid, [c000336, c000337, c000338, c000339]).
defined_as(c000336, diag_ul_lr, [triangle_magenta_large, square_green_large, square_green_small, circle_blue_large]).
defined_as(c000337, diag_ul_lr, [triangle_yellow_small, circle_green_small, triangle_green_large, triangle_blue_small]).
defined_as(c000338, diag_ul_lr, [triangle_magenta_small, square_yellow_small, triangle_green_large, triangle_magenta_small]).
defined_as(c000339, diag_ul_lr, [triangle_blue_small, circle_cyan_small, square_red_large, square_yellow_small]).
defined_as(c000340, grid, [c000341, c000342, c000343, c000344]).
defined_as(c000341, diag_ul_lr, [square_cyan_large, square_green_small, square_green_small, square_red_large]).
defined_as(c000342, diag_ul_lr, [triangle_magenta_small, square_cyan_small, square_yellow_large, square_cyan_small]).
defined_as(c000343, diag_ul_lr, [triangle_yellow_small, square_red_small, square_red_small, triangle_cyan_small]).
defined_as(c000344, diag_ul_lr, [circle_cyan_large, triangle_magenta_large, circle_blue_small, triangle_red_small]).
defined_as(c000345, grid, [c000346, c000347, c000348, c000349]).
defined_as(c000346, diag_ul_lr, [triangle_blue_large, triangle_red_large, square_magenta_large, triangle_red_large]).
defined_as(c000347, diag_ul_lr, [triangle_blue_large, triangle_red_large, triangle_yellow_small, triangle_red_large]).
defined_as(c000348, diag_ul_lr, [circle_cyan_large, square_magenta_small, square_magenta_small, triangle_red_large]).
defined_as(c000349, diag_ul_lr, [square_green_large, triangle_red_small, triangle_green_small, triangle_red_large]).
defined_as(c000350, grid, [c000351, c000352, c000353, c000354]).
defined_as(c000351, diag_ul_lr, [circle_magenta_small, square_magenta_small, circle_yellow_large, triangle_red_large]).
defined_as(c000352, diag_ul_lr, [triangle_magenta_large, circle_cyan_large, triangle_blue_large, square_cyan_small]).
defined_as(c000353, diag_ul_lr, [square_cyan_large, circle_red_large, circle_blue_large, triangle_blue_large]).
defined_as(c000354, diag_ul_lr, [circle_blue_small, triangle_magenta_large, triangle_yellow_large, square_blue_large]).
defined_as(c000355, grid, [c000356, c000357, c000358, c000359]).
defined_as(c000356, diag_ul_lr, [square_blue_small, square_yellow_small, circle_green_large, triangle_red_large]).
defined_as(c000357, diag_ul_lr, [square_green_small, triangle_magenta_small, triangle_magenta_small, triangle_red_large]).
defined_as(c000358, diag_ul_lr, [square_yellow_large, triangle_cyan_small, triangle_red_small, triangle_blue_small]).
defined_as(c000359, diag_ul_lr, [square_red_large, square_cyan_large, circle_yellow_large, triangle_red_large]).
defined_as(c000360, grid, [c000361, c000362, c000363, c000364]).
defined_as(c000361, diag_ul_lr, [square_yellow_small, triangle_cyan_small, square_yellow_large, circle_magenta_small]).
defined_as(c000362, diag_ul_lr, [square_green_large, square_blue_small, square_cyan_large, square_magenta_large]).
defined_as(c000363, diag_ul_lr, [triangle_yellow_small, circle_cyan_large, circle_yellow_large, triangle_cyan_small]).
defined_as(c000364, diag_ul_lr, [triangle_magenta_small, circle_blue_large, circle_yellow_small, circle_blue_small]).
defined_as(c000365, grid, [c000366, c000367, c000368, c000369]).
defined_as(c000366, diag_ul_lr, [circle_blue_large, circle_cyan_small, circle_cyan_large, triangle_blue_small]).
defined_as(c000367, diag_ul_lr, [square_magenta_large, triangle_magenta_small, circle_red_large, square_yellow_small]).
defined_as(c000368, diag_ul_lr, [triangle_red_large, triangle_green_large, circle_red_large, square_magenta_small]).
defined_as(c000369, diag_ul_lr, [triangle_yellow_large, triangle_red_small, triangle_magenta_large, square_green_large]).
defined_as(c000370, grid, [c000371, c000372, c000373, c000374]).
defined_as(c000371, diag_ul_lr, [circle_yellow_large, circle_yellow_large, square_blue_small, circle_blue_large]).
defined_as(c000372, diag_ul_lr, [circle_cyan_large, circle_red_small, triangle_cyan_large, triangle_magenta_small]).
defined_as(c000373, diag_ul_lr, [triangle_cyan_large, triangle_cyan_large, square_red_large, triangle_yellow_large]).
defined_as(c000374, diag_ul_lr, [square_red_large, circle_magenta_large, circle_magenta_small, circle_red_small]).
defined_as(c000375, grid, [c000376, c000377, c000378, c000379]).
defined_as(c000376, diag_ul_lr, [triangle_red_small, square_yellow_large, triangle_red_small, square_red_small]).
defined_as(c000377, diag_ul_lr, [square_magenta_large, square_magenta_large, triangle_red_small, circle_yellow_small]).
defined_as(c000378, diag_ul_lr, [circle_yellow_large, circle_cyan_large, triangle_red_large, square_magenta_small]).
defined_as(c000379, diag_ul_lr, [square_yellow_small, triangle_blue_small, triangle_red_large, square_magenta_small]).
defined_as(c000380, grid, [c000381, c000382, c000383, c000384]).
defined_as(c000381, diag_ul_lr, [square_blue_small, circle_red_large, triangle_magenta_small, triangle_yellow_large]).
defined_as(c000382, diag_ul_lr, [square_red_large, circle_red_small, square_magenta_small, square_red_small]).
defined_as(c000383, diag_ul_lr, [square_magenta_small, square_cyan_small, triangle_cyan_small, square_blue_large]).
defined_as(c000384, diag_ul_lr, [triangle_yellow_large, circle_magenta_small, circle_yellow_small, square_cyan_large]).
defined_as(c000385, grid, [c000386, c000387, c000388, c000389]).
defined_as(c000386, diag_ul_lr, [triangle_red_large, circle_magenta_small, circle_cyan_large, circle_cyan_small]).
defined_as(c000387, diag_ul_lr, [square_magenta_small, triangle_blue_small, triangle_red_small, circle_red_large]).
defined_as(c000388, diag_ul_lr, [circle_red_large, triangle_magenta_small, triangle_yellow_small, triangle_red_small]).
defined_as(c000389, diag_ul_lr, [triangle_red_large, square_yellow_small, square_blue_small, triangle_red_small]).
defined_as(c000390, grid, [c000391, c000392, c000393, c000394]).
defined_as(c000391, diag_ul_lr, [triangle_cyan_large, square_red_large, triangle_blue_small, circle_green_large]).
defined_as(c000392, diag_ul_lr, [circle_magenta_small, square_magenta_large, circle_green_large, circle_yellow_large]).
defined_as(c000393, diag_ul_lr, [triangle_yellow_large, circle_cyan_large, square_cyan_small, circle_blue_large]).
defined_as(c000394, diag_ul_lr, [triangle_cyan_small, circle_green_small, triangle_magenta_large, triangle_green_large]).
defined_as(c000395, grid, [c000396, c000397, c000398, c000399]).
defined_as(c000396, diag_ul_lr, [square_magenta_large, triangle_red_large, triangle_yellow_large, circle_yellow_small]).
defined_as(c000397, diag_ul_lr, [circle_green_large, square_red_small, square_green_large, triangle_red_small]).
defined_as(c000398, diag_ul_lr, [square_blue_small, triangle_red_large, triangle_red_small, triangle_yellow_small]).
defined_as(c000399, diag_ul_lr, [triangle_magenta_large, triangle_red_small, square_magenta_large, triangle_green_small]).
defined_as(c000400, grid, [c000401, c000402, c000403, c000404]).
defined_as(c000401, diag_ul_lr, [circle_blue_large, square_blue_large, circle_yellow_small, triangle_cyan_small]).
defined_as(c000402, diag_ul_lr, [square_cyan_small, triangle_blue_small, square_yellow_large, circle_green_large]).
defined_as(c000403, diag_ul_lr, [triangle_magenta_large, circle_cyan_small, square_magenta_small, circle_blue_large]).
defined_as(c000404, diag_ul_lr, [circle_green_large, square_green_large, triangle_magenta_large, circle_blue_small]).
defined_as(c000405, grid, [c000406, c000407, c000408, c000409]).
defined_as(c000406, diag_ul_lr, [triangle_red_small, triangle_magenta_large, square_yellow_large, circle_cyan_large]).
defined_as(c000407, diag_ul_lr, [circle_green_small, triangle_red_small, triangle_yellow_large, circle_cyan_large]).
defined_as(c000408, diag_ul_lr, [triangle_red_large, square_yellow_large, triangle_cyan_small, circle_green_small]).
defined_as(c000409, diag_ul_lr, [triangle_blue_small, circle_green_large, triangle_green_large, triangle_red_small]).
defined_as(c000410, grid, [c000411, c000412, c000413, c000414]).
defined_as(c000411, diag_ul_lr, [circle_green_large, triangle_red_large, square_yellow_large, circle_yellow_small]).
defined_as(c000412, diag_ul_lr, [triangle_blue_small, triangle_magenta_large, square_blue_large, triangle_red_small]).
defined_as(c000413, diag_ul_lr, [circle_magenta_small, square_magenta_large, circle_blue_small, triangle_red_large]).
defined_as(c000414, diag_ul_lr, [triangle_red_small, circle_blue_small, square_red_large, square_cyan_small]).
defined_as(c000415, grid, [c000416, c000417, c000418, c000419]).
defined_as(c000416, diag_ul_lr, [square_cyan_large, circle_red_small, square_cyan_small, triangle_red_small]).
defined_as(c000417, diag_ul_lr, [triangle_green_large, square_red_small, square_magenta_small, triangle_red_small]).
defined_as(c000418, diag_ul_lr, [triangle_magenta_small, circle_cyan_small, triangle_red_small, square_magenta_small]).
defined_as(c000419, diag_ul_lr, [triangle_cyan_small, square_cyan_large, triangle_red_large, circle_yellow_large]).
defined_as(c000420, grid, [c000421, c000422, c000423, c000424]).
defined_as(c000421, diag_ul_lr, [triangle_red_large, triangle_red_small, square_blue_small, triangle_red_small]).
defined_as(c000422, diag_ul_lr, [square_magenta_large, triangle_red_small, triangle_green_small, triangle_blue_large]).
defined_as(c000423, diag_ul_lr, [square_yellow_small, triangle_red_large, circle_yellow_large, square_cyan_small]).
defined_as(c000424, diag_ul_lr, [triangle_yellow_small, triangle_red_small, triangle_yellow_small, circle_blue_large]).
defined_as(c000425, grid, [c000426, c000427, c000428, c000429]).
defined_as(c000426, diag_ul_lr, [triangle_blue_large, square_cyan_large, triangle_red_small, square_cyan_large]).
defined_as(c000427, diag_ul_lr, [triangle_red_large, square_yellow_small, circle_magenta_small, triangle_red_large]).
defined_as(c000428, diag_ul_lr, [square_cyan_small, triangle_red_small, triangle_yellow_small, triangle_blue_small]).
defined_as(c000429, diag_ul_lr, [circle_cyan_large, triangle_red_large, square_blue_large, triangle_red_large]).
defined_as(c000430, grid, [c000431, c000432, c000433, c000434]).
defined_as(c000431, diag_ul_lr, [square_cyan_small, circle_yellow_large, square_blue_small, square_cyan_small]).
defined_as(c000432, diag_ul_lr, [square_green_large, triangle_yellow_small, circle_cyan_large, circle_magenta_small]).
defined_as(c000433, diag_ul_lr, [square_cyan_small, square_red_large, square_yellow_small, square_green_large]).
defined_as(c000434, diag_ul_lr, [triangle_yellow_large, triangle_green_small, square_cyan_large, circle_blue_small]).
defined_as(c000435, grid, [c000436, c000437, c000438, c000439]).
defined_as(c000436, diag_ul_lr, [triangle_magenta_large, triangle_red_large, triangle_cyan_large, triangle_cyan_small]).
defined_as(c000437, diag_ul_lr, [triangle_magenta_large, square_yellow_large, triangle_red_large, circle_red_large]).
defined_as(c000438, diag_ul_lr, [circle_blue_large, circle_magenta_large, circle_cyan_large, triangle_red_large]).
defined_as(c000439, diag_ul_lr, [square_cyan_small, square_blue_small, triangle_red_small, triangle_blue_large]).
defined_as(c000440, grid, [c000441, c000442, c000443, c000444]).
defined_as(c000441, diag_ul_lr, [triangle_red_small, triangle_green_small, square_red_small, circle_red_small]).
defined_as(c000442, diag_ul_lr, [circle_red_small, circle_green_small, triangle_green_large, triangle_red_small]).
defined_as(c000443, diag_ul_lr, [circle_red_large, triangle_blue_small, square_green_large, triangle_red_small]).
defined_as(c000444, diag_ul_lr, [circle_cyan_small, triangle_green_large, triangle_red_large, circle_green_large]).
defined_as(c000445, grid, [c000446, c000447, c000448, c000449]).
defined_as(c000446, diag_ul_lr, [triangle_red_large, triangle_cyan_small, triangle_blue_small, triangle_red_small]).
defined_as(c000447, diag_ul_lr, [triangle_green_small, circle_yellow_large, triangle_red_small, circle_red_small]).
defined_as(c000448, diag_ul_lr, [triangle_red_small, triangle_magenta_large, triangle_green_small, circle_cyan_small]).
defined_as(c000449, diag_ul_lr, [square_green_large, circle_blue_small, triangle_red_large, triangle_yellow_large]).
defined_as(c000450, grid, [c000451, c000452, c000453, c000454]).
defined_as(c000451, diag_ul_lr, [circle_red_small, triangle_red_small, square_cyan_small, circle_cyan_large]).
defined_as(c000452, diag_ul_lr, [triangle_red_small, triangle_cyan_large, square_red_small, triangle_red_large]).
defined_as(c000453, diag_ul_lr, [square_cyan_small, triangle_red_large, triangle_cyan_large, triangle_red_large]).
defined_as(c000454, diag_ul_lr, [triangle_red_small, square_cyan_large, circle_red_small, square_red_small]).
defined_as(c000455, grid, [c000456, c000457, c000458, c000459]).
defined_as(c000456, diag_ul_lr, [triangle_cyan_small, circle_red_small, triangle_red_large, circle_red_large]).
defined_as(c000457, diag_ul_lr, [triangle_red_small, circle_red_large, circle_cyan_small, square_yellow_small]).
defined_as(c000458, diag_ul_lr, [square_blue_large, square_blue_small, circle_cyan_large, circle_cyan_large]).
defined_as(c000459, diag_ul_lr, [square_green_large, square_green_small, circle_blue_large, square_yellow_small]).
defined_as(c000460, grid, [c000461, c000462, c000463, c000464]).
defined_as(c000461, diag_ul_lr, [square_blue_large, triangle_blue_large, triangle_green_small, triangle_green_large]).
defined_as(c000462, diag_ul_lr, [triangle_yellow_small, circle_blue_small, circle_green_small, circle_red_small]).
defined_as(c000463, diag_ul_lr, [square_green_large, square_red_small, circle_yellow_small, circle_magenta_small]).
defined_as(c000464, diag_ul_lr, [triangle_blue_small, square_green_large, circle_magenta_small, circle_magenta_large]).
defined_as(c000465, grid, [c000466, c000467, c000468, c000469]).
defined_as(c000466, diag_ul_lr, [triangle_magenta_large, triangle_red_large, circle_green_small, circle_blue_large]).
defined_as(c000467, diag_ul_lr, [triangle_red_large, square_magenta_small, square_blue_large, circle_magenta_large]).
defined_as(c000468, diag_ul_lr, [triangle_red_small, circle_magenta_large, triangle_green_small, triangle_yellow_large]).
defined_as(c000469, diag_ul_lr, [circle_red_small, triangle_green_small, triangle_cyan_small, triangle_red_small]).
defined_as(c000470, grid, [c000471, c000472, c000473, c000474]).
defined_as(c000471, diag_ul_lr, [square_red_small, circle_cyan_small, triangle_red_large, square_cyan_large]).
defined_as(c000472, diag_ul_lr, [square_cyan_small, circle_green_large, triangle_red_large, circle_magenta_large]).
defined_as(c000473, diag_ul_lr, [triangle_blue_small, triangle_yellow_large, square_magenta_large, triangle_red_large]).
defined_as(c000474, diag_ul_lr, [triangle_red_large, triangle_red_large, circle_red_large, triangle_blue_large]).
defined_as(c000475, grid, [c000476, c000477, c000478, c000479]).
defined_as(c000476, diag_ul_lr, [circle_magenta_small, square_yellow_large, triangle_cyan_large, square_red_large]).
defined_as(c000477, diag_ul_lr, [triangle_green_small, circle_red_large, circle_magenta_small, square_yellow_large]).
defined_as(c000478, diag_ul_lr, [square_yellow_large, circle_cyan_small, circle_cyan_small, circle_cyan_large]).
defined_as(c000479, diag_ul_lr, [triangle_blue_large, circle_magenta_small, circle_blue_large, square_green_small]).
defined_as(c000480, grid, [c000481, c000482, c000483, c000484]).
defined_as(c000481, diag_ul_lr, [triangle_blue_large, circle_cyan_small, triangle_green_large, square_magenta_small]).
defined_as(c000482, diag_ul_lr, [triangle_blue_large, square_cyan_large, square_blue_large, circle_yellow_large]).
defined_as(c000483, diag_ul_lr, [square_green_small, square_red_large, triangle_red_large, square_cyan_small]).
defined_as(c000484, diag_ul_lr, [circle_magenta_large, circle_yellow_large, triangle_magenta_large, square_blue_small]).
defined_as(c000485, grid, [c000486, c000487, c000488, c000489]).
defined_as(c000486, diag_ul_lr, [square_yellow_large, triangle_magenta_small, square_magenta_small, square_blue_small]).
defined_as(c000487, diag_ul_lr, [circle_blue_small, triangle_cyan_small, circle_cyan_small, square_green_large]).
defined_as(c000488, diag_ul_lr, [triangle_blue_small, triangle_blue_small, circle_cyan_small, square_cyan_small]).
defined_as(c000489, diag_ul_lr, [circle_magenta_small, square_yellow_large, square_cyan_small, square_cyan_small]).
defined_as(c000490, grid, [c000491, c000492, c000493, c000494]).
defined_as(c000491, diag_ul_lr, [square_yellow_large, square_yellow_small, square_magenta_large, square_green_small]).
defined_as(c000492, diag_ul_lr, [circle_green_small, square_green_small, square_red_large, circle_red_large]).
defined_as(c000493, diag_ul_lr, [circle_cyan_large, square_blue_small, triangle_yellow_large, triangle_red_large]).
defined_as(c000494, diag_ul_lr, [circle_magenta_small, square_red_small, circle_cyan_large, circle_blue_large]).
defined_as(c000495, grid, [c000496, c000497, c000498, c000499]).
defined_as(c000496, diag_ul_lr, [triangle_red_small, square_blue_large, square_blue_small, circle_green_small]).
defined_as(c000497, diag_ul_lr, [square_green_small, circle_blue_small, square_red_small, triangle_red_small]).
defined_as(c000498, diag_ul_lr, [circle_magenta_small, circle_blue_large, triangle_cyan_small, square_yellow_large]).
defined_as(c000499, diag_ul_lr, [triangle_yellow_large, circle_magenta_large, circle_magenta_small, square_yellow_small]).
sample_is(t001_s000000, c000000).
sample_is(t001_s000001, c000005).
sample_is(t001_s000002, c000010).
sample_is(t001_s000003, c000015).
sample_is(t001_s000004, c000020).
sample_is(t001_s000005, c000025).
sample_is(t001_s000006, c000030).
sample_is(t001_s000007, c000035).
sample_is(t001_s000008, c000040).
sample_is(t001_s000009, c000045).
sample_is(t001_s000010, c000050).
sample_is(t001_s000011, c000055).
sample_is(t001_s000012, c000060).
sample_is(t001_s000013, c000065).
sample_is(t001_s000014, c000070).
sample_is(t001_s000015, c000075).
sample_is(t001_s000016, c000080).
sample_is(t001_s000017, c000085).
sample_is(t001_s000018, c000090).
sample_is(t001_s000019, c000095).
sample_is(t001_s000020, c000100).
sample_is(t001_s000021, c000105).
sample_is(t001_s000022, c000110).
sample_is(t001_s000023, c000115).
sample_is(t001_s000024, c000120).
sample_is(t001_s000025, c000125).
sample_is(t001_s000026, c000130).
sample_is(t001_s000027, c000135).
sample_is(t001_s000028, c000140).
sample_is(t001_s000029, c000145).
sample_is(t001_s000030, c000150).
sample_is(t001_s000031, c000155).
sample_is(t001_s000032, c000160).
sample_is(t001_s000033, c000165).
sample_is(t001_s000034, c000170).
sample_is(t001_s000035, c000175).
sample_is(t001_s000036, c000180).
sample_is(t001_s000037, c000185).
sample_is(t001_s000038, c000190).
sample_is(t001_s000039, c000195).
sample_is(t001_s000040, c000200).
sample_is(t001_s000041, c000205).
sample_is(t001_s000042, c000210).
sample_is(t001_s000043, c000215).
sample_is(t001_s000044, c000220).
sample_is(t001_s000045, c000225).
sample_is(t001_s000046, c000230).
sample_is(t001_s000047, c000235).
sample_is(t001_s000048, c000240).
sample_is(t001_s000049, c000245).
sample_is(t001_s000050, c000250).
sample_is(t001_s000051, c000255).
sample_is(t001_s000052, c000260).
sample_is(t001_s000053, c000265).
sample_is(t001_s000054, c000270).
sample_is(t001_s000055, c000275).
sample_is(t001_s000056, c000280).
sample_is(t001_s000057, c000285).
sample_is(t001_s000058, c000290).
sample_is(t001_s000059, c000295).
sample_is(t001_s000060, c000300).
sample_is(t001_s000061, c000305).
sample_is(t001_s000062, c000310).
sample_is(t001_s000063, c000315).
sample_is(t001_s000064, c000320).
sample_is(t001_s000065, c000325).
sample_is(t001_s000066, c000330).
sample_is(t001_s000067, c000335).
sample_is(t001_s000068, c000340).
sample_is(t001_s000069, c000345).
sample_is(t001_s000070, c000350).
sample_is(t001_s000071, c000355).
sample_is(t001_s000072, c000360).
sample_is(t001_s000073, c000365).
sample_is(t001_s000074, c000370).
sample_is(t001_s000075, c000375).
sample_is(t001_s000076, c000380).
sample_is(t001_s000077, c000385).
sample_is(t001_s000078, c000390).
sample_is(t001_s000079, c000395).
sample_is(t001_s000080, c000400).
sample_is(t001_s000081, c000405).
sample_is(t001_s000082, c000410).
sample_is(t001_s000083, c000415).
sample_is(t001_s000084, c000420).
sample_is(t001_s000085, c000425).
sample_is(t001_s000086, c000430).
sample_is(t001_s000087, c000435).
sample_is(t001_s000088, c000440).
sample_is(t001_s000089, c000445).
sample_is(t001_s000090, c000450).
sample_is(t001_s000091, c000455).
sample_is(t001_s000092, c000460).
sample_is(t001_s000093, c000465).
sample_is(t001_s000094, c000470).
sample_is(t001_s000095, c000475).
sample_is(t001_s000096, c000480).
sample_is(t001_s000097, c000485).
sample_is(t001_s000098, c000490).
sample_is(t001_s000099, c000495).
:-end_bg.

:-begin_in_pos.

valid(t001_s000002).
valid(t001_s000003).
valid(t001_s000005).
valid(t001_s000006).
valid(t001_s000007).
valid(t001_s000008).
valid(t001_s000009).
valid(t001_s000010).
valid(t001_s000014).
valid(t001_s000015).
valid(t001_s000016).
valid(t001_s000018).
valid(t001_s000019).
valid(t001_s000020).
valid(t001_s000022).
valid(t001_s000023).
valid(t001_s000024).
valid(t001_s000025).
valid(t001_s000026).
valid(t001_s000027).
valid(t001_s000029).
valid(t001_s000032).
valid(t001_s000033).
valid(t001_s000034).
valid(t001_s000035).
valid(t001_s000036).
valid(t001_s000038).
valid(t001_s000039).
valid(t001_s000040).
valid(t001_s000041).
valid(t001_s000042).
valid(t001_s000044).
valid(t001_s000045).
valid(t001_s000046).
valid(t001_s000047).
valid(t001_s000048).
valid(t001_s000050).
valid(t001_s000051).
valid(t001_s000052).
valid(t001_s000056).
valid(t001_s000057).
valid(t001_s000060).
valid(t001_s000062).
valid(t001_s000063).
valid(t001_s000064).
valid(t001_s000065).
valid(t001_s000066).
valid(t001_s000068).
valid(t001_s000069).
valid(t001_s000070).
valid(t001_s000071).
valid(t001_s000073).
valid(t001_s000075).
valid(t001_s000077).
valid(t001_s000079).
valid(t001_s000081).
valid(t001_s000082).
valid(t001_s000083).
valid(t001_s000084).
valid(t001_s000085).
valid(t001_s000087).
valid(t001_s000088).
valid(t001_s000089).
valid(t001_s000090).
valid(t001_s000091).
valid(t001_s000093).
valid(t001_s000094).
valid(t001_s000096).
valid(t001_s000098).
valid(t001_s000099).
:-end_in_pos.

:-begin_in_neg.

valid(t001_s000000).
valid(t001_s000001).
valid(t001_s000004).
valid(t001_s000011).
valid(t001_s000012).
valid(t001_s000013).
valid(t001_s000017).
valid(t001_s000021).
valid(t001_s000028).
valid(t001_s000030).
valid(t001_s000031).
valid(t001_s000037).
valid(t001_s000043).
valid(t001_s000049).
valid(t001_s000053).
valid(t001_s000054).
valid(t001_s000055).
valid(t001_s000058).
valid(t001_s000059).
valid(t001_s000061).
valid(t001_s000067).
valid(t001_s000072).
valid(t001_s000074).
valid(t001_s000076).
valid(t001_s000078).
valid(t001_s000080).
valid(t001_s000086).
valid(t001_s000092).
valid(t001_s000095).
valid(t001_s000097).
:-end_in_neg.
