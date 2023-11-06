body_pred(symmetric_list, 1).

body_pred(forall_same_color, 2).
body_pred(forall_same_shape, 2).
body_pred(extract_two_different, 3).


body_pred(perpendicular, 1).
body_pred(forall_palindrome, 1).
body_pred(pseudo_palindrome, 1).
body_pred(forall_palindrome2, 1).
body_pred(minimum_count_number, 2).
body_pred(minimum_count_child, 2).
body_pred(exists_red_square, 1).
body_pred(rotate, 2).
body_pred(majority_color, 2).
body_pred(majority_shape, 2).

type(symmetric_list, (list_t,)).

type(forall_same_color, (term_t, color_t)).
type(forall_same_shape, (term_t, shape_t)).
type(extract_two_different, (term_t, term_t, term_t)).

type(perpendicular, (term_t,)).
type(forall_palindrome, (list_t,)).
type(pseudo_palindrome, (list_t,)).
type(forall_palindrome2, (list_t,)).
type(minimum_count_number, (list_t, int)).
type(minimum_count_child, (list_t, term_t)).
type(exists_red_square, (list_t,)).
type(rotate, (list_t, list_t)).
type(majority_color, (color_t, list_t)).
type(majority_shape, (shape_t, list_t)).

direction(symmetric_list, (in,)).

direction(forall_same_color, (in, out)).
direction(forall_same_shape, (in, out)).
direction(extract_two_different, (in, out, out)).

direction(perpendicular, (in,)).
direction(forall_palindrome, (in,)).
direction(pseudo_palindrome, (in,)).
direction(forall_palindrome2, (in,)).
direction(minimum_count_number, (in,out)).
direction(minimum_count_child, (in,out)).
direction(exists_red_square, (in,)).
direction(rotate, (in,out)).
direction(majority_color, (out,in)).
direction(majority_shape, (out,in)).
