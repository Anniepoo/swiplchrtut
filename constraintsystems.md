Tutorial - CHR Constraint Handling Rules - Constraint Systems
=============================================================
Anne Ogborn <annie66us@yahoo.com>
:Author Initials: AO
:toc2:
:icons:
:numbered:
:website: http://www.pathwayslms.com/swipltuts/
:theme: pathways

link:/swipltuts/index.html[Up to All Tutorials]

The material in this chapter depends on understanding constraint systems.

If you want to understand making constraint systems, at least read the intro chapter of my [clp(fd) tutorial](/swipltuts/clpfd/clpfd.html), the Prolog clpfd library notes,
and the entries for attributed variables in the SWI-Prolog manual.

.Review
.A Simple Constraint System

Review
------

To review - CHR constraints are **active** when added to the store.

While there is an **active** constraint, CHR looks for a rule to fire, until no rule involving that constraint can fire. The constraint is then deactivated, and control returned.

Since no rule was firing (at this stack level) before the constraint was added, only
rules that contain the active constraint need be searched.

There is an important exception to this rule. If the **guard** encounters `ground/1`, the **rule**
(not the constraints) will **reactivate** when the argument of `ground/1` is **grounded**.

----
more_than_3(N) <=>  ground(N), N > 3 | true.

?-more_than_3(X),writeln('middle'),X = 2.
middle
false.
----

I would note that in this respect the CHR implementation in SWI-Prolog is not very good. Many other
Prologs also allow a variety of other predicates to act in this manner - `nonvar/1`, the arithmetic operators,
and so on. 


When CHR fires a rule as a result of reactivation, the guard 
If one of the rules fails, then the **unification fails in the original Prolog that caused the unification**.

This allows us to build **constraint systems**.  If you're not familiar with constraint systems, check out [my clp(fd) tutorial](http://www.pathwayslms.com/swipltuts/clpfd/clpfd.html).
Note that the above code says 'middle', and only fails when X is bound.

Since we can later change the constraint we can develop very efficient constraint checkers.
If we apply `more_than_4` to X then we _subsume_ `more_than_3`. We can discard the weaker constraint.

----
more_than_4(X) \ more_than_3(X) <=> true.
----


Since we can later change the constraint we can develop very efficient constraint checkers.
If we apply `more_than_4` to X then we _subsume_ `more_than_3`. We can discard the weaker constraint.

----
more_than_4(X) \ more_than_3(X) <=> true.
----


