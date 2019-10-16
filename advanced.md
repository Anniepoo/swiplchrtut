Tutorial - CHR Constraint Handling Rules - Modes, Types, Performance
====================================================================
Anne Ogborn <annie66us@yahoo.com>
:Author Initials: AO
:toc2:
:icons:
:numbered:
:website: http://www.pathwayslms.com/swipltuts/
:theme: pathways

link:/swipltuts/index.html[Up to All Tutorials]


Advanced CHR syntax
* pragmas
* modes and types

Performance
-----------

----
fc(X) :-
    functor(X, XF, N),
    functor(Y, XF, N),
    find_chr_constraint(Y),
    subsumer(X, Y).

subsumer(A, B) :-
    copy_term(B, BCopy)
    , catch(A = B, _, fail)
    , =@=(B, BCopy)
    .

    /*
?- fc(my_con(A, 3, B)),
fc(my_con(B, 3, C)).

% above is O(n^2)
:- chr_constraint my_con/3.

% above is linear
:- chr_constraint my_con(+dense_int, +dense_int, +dense_int).
----


don't chr for no reason.


