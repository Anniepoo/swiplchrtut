:- module(chrtest, []).
:- use_module(library(chr)).

:- chr_constraint foo/1, get_foo/1, recurse_foo/1.

load_it :-
    foo(1),
    foo(3),
    foo(7).

/*
% no backtrack get
foo(X) \ get_foo(Y) <=>
         X = Y.
get_foo(_) <=> fail.
*/

/* doesnt work
foo(X), recurse_foo(Y) ==>
       Y = [ X | Rest  ],
       recurse_foo(Rest).
recurse_foo(_) <=> true.
*/

all_foos(L) :-
    nb_setval(all_foos_val, []),
    af(L).

af(_) :-
    find_chr_constraint(foo(X)),
    nb_getval(all_foos_val, OldL),
    nb_setval(all_foos_val, [X | OldL]),
    fail.
af(L) :-
    nb_getval(all_foos_val, L).

:- chr_constraint gather_foos_together/1, get_thy_foos/1.

foo(X) \ gather_foos_together(Old) <=> gather_foos_together([X | Old]).

gather_foos_together(Foos), get_thy_foos(Y) ==> Foos = Y.
% get_thy_foos(_) <=> true.   don't do this


:- chr_constraint get_foo_list/1.

foo(X), get_foo_list(L) ==>
          L=[X|L1], get_foo_list(L1).
get_foo_list(L) <=> L=[].


:- chr_constraint one_foo/1, collect_foo/1.

% copy constraints to be collected
foo(X), get_foo(_) ==> one_foo(X).
get_foo(L) <=> collect_foo(L).

% collect and remove copied constraints
one_foo(X), collect_foo(L) <=>
          L=[X|L1], collect_foo(L1).
collect_foo(L) <=> L=[].


:- chr_constraint   i_delay/1, i_throw/1.

i_throw(X) ==> X > 3 | writeln('fired'). % throws uninstantiated
i_delay(_) ==> writeln('firstrule').
i_delay(X) ==> ground(X), X > 3 | writeln(fired).

:- chr_constraint partial_bound/1.

partial_bound(X) <=> nonvar(X) | is_list(X).


:- chr_constraint card/1, straight/0.

/* slow version
card(A), card(B), card(C), card(D), card(E) ==>
         succ(A,B),
         succ(B,C),
         succ(C,D),
         succ(D,E) | straight.
*/

:- chr_constraint adj_pair/2, adj_triple/2.

card(A), card(B) ==> succ(A,B) | adj_pair(A,B).
adj_pair(A,B),card(C) ==> succ(B,C) | adj_triple(A, C).
adj_pair(_,B),adj_triple(C,_) <=> succ(B,C) | straight.
adj_triple(_,B),adj_pair(C,_) <=> succ(B,C) | straight.


:- chr_constraint do_exprs_work/0, an_expr/1.

do_exprs_work <=> an_expr(1+2).

:- use_module(moduleb).

:- chr_constraint in_a/0.

in_a ==> in_b.

:- chr_constraint call_private/0.

call_private ==> moduleb:private_b.














