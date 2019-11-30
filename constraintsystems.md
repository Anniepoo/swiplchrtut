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
link:index.html[Introduction]
link:basics.html[Basics]
link:examples.html[Examples]
link:constraintsystems.html[Constraint Systems]
link:advanced.html[Advanced]
link:final.html[Final]

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

`nonvar/1` works the same way.

I would note that in this respect the CHR implementation in SWI-Prolog is not very good. Many other
Prologs also allow a variety of other predicates to act in this manner - the arithmetic comparison operators,
and so on. 

If one of the reactivated rules fails, then the **unification fails in the original Prolog that caused the unification**.

This allows us to build **constraint systems**.  If you're not familiar with constraint systems, check out 
link:http://www.pathwayslms.com/swipltuts/clpfd/clpfd.html[my clp(fd) tutorial].
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

A simple constraint system
--------------------------

Let's make a constraint system that works over lists.

We'll start by providing a variable that just ensures that a variable is only grounded to a list.

----
:- chr_constraint cl_list/1.                       <1>

cl_list(X) <=> nonvar(X) | is_list(X).             <2>

go :- cl_list(X),                                  <3>
      writeln('x not bound'),                      <4>
      X=[_].                                       <5>
gofail :- cl_list(X),                              <3>
          writeln('x not bound'),                  <4>
          X=3.                                     <6>
----
<1> define a constraint
<2> if a `cl_list/1` is in the store, when X is partially grounded (nonvar), it must be a list.
<3> test if it works. `cl_list/1` doesn't fire rule 2 when the constraint is added. 
<4> writeln to prove we get past the `cl_list/1` call.
<5> Only when X is partially ground is the rule checked - and it's indeed a list. Notice if you run this that the constraint is not in the store, we used a simplification (`<=>`) rule.
<6> This time, when we finally run the rule body it fails because 3 is not a list.

Now let's add a minimum length constraint.

----
:- chr_constraint cl_min_len/1.

cl_min_len(X, MinLen) <=>           <1>
         nonvar(X),
         ground(MinLen) |           <2>
         is_list(X), 
         number(MinLen),
         length(X, Len), 
         Len >= MinLen.             <3>
----
<1> Use a simplification rule, since when it fires we don't need the constraint after.
<2> Check that both are bound before firing the rule.
<3> Now make sure the constraint holds when the rule fires.

[Exercise]
.Exercise - Ascending
=====================================================================
Add a `cl_ascending` constraint that demands the elements in the list
are in ascending order in the standard Prolog collating order.
=====================================================================

Suppose a user does:

----
blah :-
   cl_min_len(X, 3),
   cl_min_len(X, 5),
   ... more ...
----

We don't really need the `cl_min_len(X, 3)` any more.

----
cl_min_len(X, A) \ cl_min_len(X, B) <=> A > B | true.
----

Now we don't have an extra constraint to check.


[Exercise]
.Exercise - Late binding the minimums
=====================================================================
Test that this works if you don't have the minimums bound when you
add the constraints, but only ground them later.
=====================================================================

[Exercise]
.Exercise - Build a constraint system
=====================================================================
Project - 
Make a constraint system for numbers as booleans.

* bits_on(Bits, X)  % all bits set in Bits must be set in X
* bits_off(Bits, X) % all bits 0 in Bits must be 0 in X

At minimum make it:

* replace multiple bits_on with one bits_on
* replace multiple bits_off with one bits_off
* fail if conflicting bits_on and bits_off are added
* fail if X is grounded to other than an integer
* fail if X doesn't match the constraints
* remove all the constraints for X after they're no longer needed.

=====================================================================

Conclusion
----------

Building constraint systems was the original application of CHR, and is still an important application.
If you don't see yourself using constraint systems, consider that dynamic types are just constraint systems.

You're now ready to move on to the 
link:advanced.html[Advanced]
material.

If you haven't worked through the 
link:examples.html[Examples]
chapter, I suggest doing that before doing the 
advanced material.

