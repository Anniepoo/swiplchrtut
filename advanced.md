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

This chapter covers some advanced CHR topics.

Advanced CHR syntax
* pragmas
* modes and types
* performance
* tools 
pragmas
name
performance
modes
types
tools - all the recursive top level, ss, noss etc tools


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

?- fc(my_con(A, 3, B)),
fc(my_con(B, 3, C)).

% above is O(n^2)
:- chr_constraint my_con/3.

% above is linear
:- chr_constraint my_con(+dense_int, +dense_int, +dense_int).
----

repeat this
single threaded
mention falco's server and the LD45 server


don't chr for no reason.

pragmas
name
performance
modes
types
tools - all the recursive top level, ss, noss etc tools

all in here
https://dtai.cs.kuleuven.be/CHR/files/CHR_cheatsheet.pdf


