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

Much of the material in this chapter is taken from the [CHR Cheatsheet](https://dtai.cs.kuleuven.be/CHR/files/CHR_cheatsheet.pdf) by Amira Zaki, Thom Fr ̈uhwirth, and Jon Sneyers.

Because this document covers much of what I want to cover in this chapter better than I could,
I'll be referring the student to it repeatedly.

Advanced CHR
* CHR options
* modes and types
* pragmas
* performance
* tools

CHR Options
-----------

You can set CHR options with the directive `chr_options/2`. Options are

* `check_guard_bindings` (on/off) - make guards that bind variables error the CHR compiler (otherwise their behavior is undefined).
* `optimize` (full/off) - do optimization during compilation (which disables debugging)
* `debug` (on/off) - generate debug information (which carries a performance penalty)

See CHR Cheatsheet for more info.

Modes and Types
---------------

The CHR Cheatsheet coverage of this material is not as good as the [CHR page on swi-prolog.org](https://www.swi-prolog.org/pldoc/man?section=practical). I suggest reading that first, particularly the section on types and modes, and only then read the material in the CHR Cheatsheet.

User defined types in particular, and the consequences of defining mode and type, are best described by the SWI-Prolog documentation.

This material is important both for performance and for getting both static and dynamic type checking.

[Exercise]
.Exercise - Modes and Types
=====================================================================
Take several of the CHR programs you will have accumulated by now
and define the CHR constraints with mode and type definitions. 
Measure performance on large data sets before and after.

Include at least one use of `chr_type/1`.
=====================================================================

Pragmas
-------

The student is referred to the CHR cheatsheet for this material. My experience has
been that adding pragmas is a black art. The CHR mailing list may be your best resource
for doing this efficiently.

Modules
-------

<<TODO>>research

Performance
-----------

[Exercise]
.Exercise - performance measurement
=====================================================================
By now you will have accumulated a number of CHR programs as exercises.

Choose some that can operate on an arbitrary amount of data and greatly
increase that amount. Try running them inside `timer/1`. Also try running
the profiler.
=====================================================================

Use Modes and Types
~~~~~~~~~~~~~~~~~~~

Alan Baljeu provided this snippet:

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
----

It runs in O(n^2) if you define the CHR constraint

----
:- chr_constraint my_con/3.
----

If you instead do this, it is linear.
----
:- chr_constraint my_con(+dense_int, +dense_int, +dense_int).
----

Avoid Having Many Heads
~~~~~~~~~~~~~~~~~~~~~~~

Consider this constraint system that looks for a _straight_ in poker.
Assume the face cards are represented by integers:

----
:- chr_constraint card/1, straight/0.

card(A), card(B), card(C), card(D), card(E) ==> 
         succ(A,B),
         succ(B,C),
         succ(C,D),
         succ(D,E) | straight.
----

This apparently reasonable constraint operates VERY slowly. If you have 7 cards, it will arrange the cards in **2,520 ways** and rerun the guard for each.

----
?- time(card(2)),card(4),card(3),card(5),time(card(6)).
% 39 inferences, 0.000 CPU in 0.000 seconds (95% CPU, 1874459 Lips)
% 8,378 inferences, 0.001 CPU in 0.001 seconds (100% CPU, 9221480 Lips)
----

Here's a faster version

----
:- chr_constraint adj_pair/2, adj_triple/2.
:- chr_constraint card/1, straight/0.

card(A), card(B) ==> succ(A,B) | adj_pair(A,B).
adj_pair(A,B),card(C) ==> succ(B,C) | adj_triple(A, C).
adj_pair(_,B),adj_triple(C,_) <=> succ(B,C) | straight.
adj_triple(_,B),adj_pair(C,_) <=> succ(B,C) | straight.
----

It runs significantly faster:

----
?- time(card(2)),card(4),card(3),card(5),time(card(6)).
% 81 inferences, 0.000 CPU in 0.000 seconds (95% CPU, 842854 Lips)
% 527 inferences, 0.000 CPU in 0.000 seconds (99% CPU, 2187530 Lips)
----

Don't Use CHR if Prolog Will Do
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The CHR compiler is efficient for an automatic code generator, but there is 
an inherent performance overhead in using it.

[Exercise]
.Exercise - Look at the generated code
=====================================================================
The CHR system produces a Prolog predicate for each chr_constraint.

Use `listing/1` to look at the code generated by some chr_constraints.

On my machine `adj_triple/2` from the last section expands to 22 lines
of Prolog in a single clause.
=====================================================================


Adjust Which arguments are Indexed
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Section 1.3 of the CHR Cheatsheet describes manually tweaking argument indexing. If you
get this working, you're doing better than the author of this tutorial, and she would
beg you to please explain it to her.

One strong lesson to be gleaned from the section is that `dense_int` is much faster, particularly
on SWI-Prolog, and it's better to map your data into a dense subset of integers instead of
storing atoms.

Tools
-----

There are some useful tools available for CHR.

[WebCHR](<<TODO>>) is useful for experimenting with CHR, and has some more academic
oriented examples.

At times CHR's single threaded nature can be painful. 
[Falco Nogatz](<<TODO>>) has created a server that single threads CHR calls.
For many applications, it might be better to start with the [Ludum Dare Team 45 Server](<<TODO>>)
largely copied from Falco's, but turned into an HTTP server.

[Michael Richter](<<TODO>>) has created a universal version of `get_foo/1`.

The official [CHR Website](<<TODO>>) is a resource for both information and tools.

Some of these tools use the optional **name** you can apply to CHR rules:

----
my_rule @ foo <=> bar.
----

<<TODO>>Using the chr debugger.

<<TODO>>all the recursive top level, ss, noss etc tools

<<TODO>> Research tools

<<TODO>> resources - off chr website, swi-prolog.org, and wikipedia.



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


