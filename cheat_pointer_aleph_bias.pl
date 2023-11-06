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
