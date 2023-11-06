:- use_module(aleph).
:- if(current_predicate(use_rendering/1)).
:- use_rendering(prolog).
:- endif.
:- aleph.
:-style_check(-discontiguous).
:- aleph_set(verbosity, 0).
:- aleph_set(newvars, 15).
:- aleph_set(clauses, 4).
:- aleph_set(clauselength, 12).
:- aleph_set(min_acc, 0.5).
:- aleph_set(noise, 5).
:- aleph_set(i, 5).
:- aleph_set(min_pos, 2).
:- aleph_set(depth, 15).
:- aleph_set(nodes, 20000).
:- aleph_set(timeout, 1800).
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

atomic_obj(circle_blue_large).
atomic_obj(circle_cyan_large).
atomic_obj(circle_green_large).
atomic_obj(circle_green_small).
atomic_obj(circle_magenta_small).
atomic_obj(circle_red_small).
atomic_obj(circle_yellow_large).
atomic_obj(square_blue_large).
atomic_obj(square_blue_small).
atomic_obj(square_cyan_large).
atomic_obj(square_cyan_small).
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
atomic_obj(triangle_magenta_small).
atomic_obj(triangle_red_large).
atomic_obj(triangle_red_small).
atomic_obj(triangle_yellow_large).
atomic_obj(triangle_yellow_small).
defined_as(c000000, random, [c000001]).
defined_as(c000001, stack, [triangle_magenta_small, square_blue_small]).
defined_as(c000002, random, [c000003]).
defined_as(c000003, side_by_side_reduce_bb, [circle_green_large, square_magenta_small]).
defined_as(c000004, random, [c000005]).
defined_as(c000005, stack, [square_magenta_small, circle_green_large]).
defined_as(c000006, random, [c000007]).
defined_as(c000007, diag_ll_ur, [circle_cyan_large, square_yellow_small]).
defined_as(c000008, random, [c000009]).
defined_as(c000009, stack, [triangle_cyan_large, square_blue_large]).
defined_as(c000010, random, [c000011]).
defined_as(c000011, stack, [triangle_green_small, square_yellow_large]).
defined_as(c000012, random, [c000013]).
defined_as(c000013, stack, [triangle_cyan_large, square_yellow_large]).
defined_as(c000014, random, [c000015]).
defined_as(c000015, stack_reduce_bb, [triangle_magenta_small, triangle_blue_large]).
defined_as(c000016, random, [c000017]).
defined_as(c000017, grid, [triangle_green_small, square_green_small]).
defined_as(c000018, random, [c000019]).
defined_as(c000019, side_by_side, [circle_yellow_large, triangle_red_large]).
defined_as(c000020, random, [c000021]).
defined_as(c000021, stack, [triangle_blue_small, square_magenta_large]).
defined_as(c000022, random, [c000023]).
defined_as(c000023, stack, [triangle_blue_large, square_blue_large]).
defined_as(c000024, random, [c000025]).
defined_as(c000025, side_by_side, [circle_blue_large, square_blue_small]).
defined_as(c000026, random, [c000027]).
defined_as(c000027, diag_ul_lr, [square_magenta_small, circle_magenta_small]).
defined_as(c000028, random, [c000029]).
defined_as(c000029, stack, [triangle_magenta_small, square_cyan_large]).
defined_as(c000030, random, [c000031]).
defined_as(c000031, stack_reduce_bb, [triangle_cyan_small, circle_magenta_small]).
defined_as(c000032, random, [c000033]).
defined_as(c000033, stack, [triangle_blue_large, square_magenta_large]).
defined_as(c000034, random, [c000035]).
defined_as(c000035, stack, [triangle_red_small, square_red_large]).
defined_as(c000036, random, [c000037]).
defined_as(c000037, diag_ll_ur, [triangle_yellow_small, circle_green_small]).
defined_as(c000038, random, [c000039]).
defined_as(c000039, stack, [triangle_cyan_small, square_blue_large]).
defined_as(c000040, random, [c000041]).
defined_as(c000041, diag_ll_ur, [circle_blue_large, square_red_small]).
defined_as(c000042, random, [c000043]).
defined_as(c000043, stack_reduce_bb, [square_red_large, circle_cyan_large]).
defined_as(c000044, random, [c000045]).
defined_as(c000045, diag_ul_lr, [triangle_green_small, square_blue_large]).
defined_as(c000046, random, [c000047]).
defined_as(c000047, stack, [triangle_blue_large, square_blue_small]).
defined_as(c000048, random, [c000049]).
defined_as(c000049, stack, [triangle_yellow_large, square_yellow_small]).
defined_as(c000050, random, [c000051]).
defined_as(c000051, stack, [triangle_blue_large, square_magenta_small]).
defined_as(c000052, random, [c000053]).
defined_as(c000053, stack, [triangle_yellow_small, square_magenta_small]).
defined_as(c000054, random, [c000055]).
defined_as(c000055, stack, [triangle_blue_small, square_cyan_small]).
defined_as(c000056, random, [c000057]).
defined_as(c000057, stack, [triangle_magenta_small, square_yellow_large]).
defined_as(c000058, random, [c000059]).
defined_as(c000059, diag_ul_lr, [triangle_magenta_small, square_red_large]).
defined_as(c000060, random, [c000061]).
defined_as(c000061, stack, [triangle_yellow_large, square_red_small]).
defined_as(c000062, random, [c000063]).
defined_as(c000063, stack, [triangle_green_small, square_blue_large]).
defined_as(c000064, random, [c000065]).
defined_as(c000065, stack, [triangle_magenta_small, square_red_small]).
defined_as(c000066, random, [c000067]).
defined_as(c000067, stack, [triangle_yellow_small, square_blue_large]).
defined_as(c000068, random, [c000069]).
defined_as(c000069, stack, [circle_blue_large, square_cyan_small]).
defined_as(c000070, random, [c000071]).
defined_as(c000071, stack, [triangle_blue_large, square_yellow_large]).
defined_as(c000072, random, [c000073]).
defined_as(c000073, grid, [triangle_red_large, square_yellow_small]).
defined_as(c000074, random, [c000075]).
defined_as(c000075, diag_ll_ur, [circle_red_small, circle_red_small]).
defined_as(c000076, random, [c000077]).
defined_as(c000077, grid, [circle_green_small, triangle_red_small]).
defined_as(c000078, random, [c000079]).
defined_as(c000079, stack, [triangle_blue_large, square_cyan_large]).
defined_as(c000080, random, [c000081]).
defined_as(c000081, side_by_side_reduce_bb, [triangle_blue_small, square_yellow_small]).
defined_as(c000082, random, [c000083]).
defined_as(c000083, stack, [triangle_yellow_small, square_cyan_large]).
defined_as(c000084, random, [c000085]).
defined_as(c000085, diag_ll_ur, [triangle_green_large, triangle_red_large]).
defined_as(c000086, random, [c000087]).
defined_as(c000087, stack, [triangle_yellow_large, square_green_small]).
defined_as(c000088, random, [c000089]).
defined_as(c000089, side_by_side, [square_red_small, square_yellow_large]).
defined_as(c000090, random, [c000091]).
defined_as(c000091, stack, [square_red_large, circle_magenta_small]).
defined_as(c000092, random, [c000093]).
defined_as(c000093, stack, [triangle_red_large, square_red_large]).
defined_as(c000094, random, [c000095]).
defined_as(c000095, stack, [triangle_green_small, square_red_large]).
defined_as(c000096, random, [c000097]).
defined_as(c000097, stack, [square_red_small, square_blue_large]).
defined_as(c000098, random, [c000099]).
defined_as(c000099, side_by_side, [circle_green_small, triangle_cyan_large]).
sample_is(t014_s000000, c000000).
sample_is(t014_s000001, c000002).
sample_is(t014_s000002, c000004).
sample_is(t014_s000003, c000006).
sample_is(t014_s000004, c000008).
sample_is(t014_s000005, c000010).
sample_is(t014_s000006, c000012).
sample_is(t014_s000007, c000014).
sample_is(t014_s000008, c000016).
sample_is(t014_s000009, c000018).
sample_is(t014_s000010, c000020).
sample_is(t014_s000011, c000022).
sample_is(t014_s000012, c000024).
sample_is(t014_s000013, c000026).
sample_is(t014_s000014, c000028).
sample_is(t014_s000015, c000030).
sample_is(t014_s000016, c000032).
sample_is(t014_s000017, c000034).
sample_is(t014_s000018, c000036).
sample_is(t014_s000019, c000038).
sample_is(t014_s000020, c000040).
sample_is(t014_s000021, c000042).
sample_is(t014_s000022, c000044).
sample_is(t014_s000023, c000046).
sample_is(t014_s000024, c000048).
sample_is(t014_s000025, c000050).
sample_is(t014_s000026, c000052).
sample_is(t014_s000027, c000054).
sample_is(t014_s000028, c000056).
sample_is(t014_s000029, c000058).
sample_is(t014_s000030, c000060).
sample_is(t014_s000031, c000062).
sample_is(t014_s000032, c000064).
sample_is(t014_s000033, c000066).
sample_is(t014_s000034, c000068).
sample_is(t014_s000035, c000070).
sample_is(t014_s000036, c000072).
sample_is(t014_s000037, c000074).
sample_is(t014_s000038, c000076).
sample_is(t014_s000039, c000078).
sample_is(t014_s000040, c000080).
sample_is(t014_s000041, c000082).
sample_is(t014_s000042, c000084).
sample_is(t014_s000043, c000086).
sample_is(t014_s000044, c000088).
sample_is(t014_s000045, c000090).
sample_is(t014_s000046, c000092).
sample_is(t014_s000047, c000094).
sample_is(t014_s000048, c000096).
sample_is(t014_s000049, c000098).
:-end_bg.

:-begin_in_pos.

valid(t014_s000001).
valid(t014_s000003).
valid(t014_s000004).
valid(t014_s000015).
valid(t014_s000021).
valid(t014_s000022).
valid(t014_s000024).
:-end_in_pos.

:-begin_in_neg.

valid(t014_s000000).
valid(t014_s000002).
valid(t014_s000005).
valid(t014_s000006).
valid(t014_s000007).
valid(t014_s000008).
valid(t014_s000009).
valid(t014_s000010).
valid(t014_s000011).
valid(t014_s000012).
valid(t014_s000013).
valid(t014_s000014).
valid(t014_s000016).
valid(t014_s000017).
valid(t014_s000018).
valid(t014_s000019).
valid(t014_s000020).
valid(t014_s000023).
:-end_in_neg.
