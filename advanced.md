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
link:index.html[Introduction]
link:basics.html[Basics]
link:examples.html[Examples]
link:constraintsystems.html[Constraint Systems]
link:advanced.html[Advanced]
link:final.html[Final]

This chapter covers some advanced CHR topics.

Much of the material in this chapter is taken from the 
link:https://dtai.cs.kuleuven.be/CHR/files/CHR_cheatsheet.pdf[CHR Cheatsheet] by Amira Zaki, Thom Frühwirth, and Jon Sneyers.

Because this document covers much of what I want to cover in this chapter better than I could,
I'll be referring the student to it repeatedly.

Advanced CHR
* CHR options
* modes and types
* pragmas
* modules
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

Providing **mode** (in, out, either + - ?) and **type** information can speed up CHR performance, and provides type checking for program correctness.

The CHR Cheatsheet coverage of this material is not as good as the 
link:https://www.swi-prolog.org/pldoc/man?section=practical[CHR page on swi-prolog.org]
. I suggest reading that first, particularly the section on types and modes, and only then read the material in the CHR Cheatsheet.

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

So far we've not discussed modules.

When a CHR constraint is defined in a module, it is module qualified.

The usual module/use_module/export pattern works:

in `modulea.pl`
----
:- use_module(moduleb).

:- chr_constraint in_a/0.

in_a ==> in_b.

:- chr_constraint call_private/0.

call_private ==> moduleb:private_b.
----

in `moduleb.pl`
----
:- module(moduleb, [in_b/0]).

:- use_module(library(chr)).

:-chr_constraint in_b/0, private_b/0.

in_b ==> writeln('made in_b').

private_b ==> writeln('in private b').
----

The predicate `find_chr_constraint` doesn't handle modules. Use 
link:https://www.swi-prolog.org/pldoc/doc_for?object=chr_runtime%3Acurrent_chr_constraint/1[`current_chr_constraint`]

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
    find_chr_constraint(Y),  % use current_chr_constraint instead in new code
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

This apparently reasonable constraint operates VERY slowly. If you have 7 cards, as in _7 card stud_, it will arrange the cards in **2,520 ways** and rerun the guard for each, since card(A) can be any of 7 cards, card(B) any of the remaining 6, and so on.

----
?- time(card(2)),card(4),card(3),card(5),time(card(6)).
% 39 inferences, 0.000 CPU in 0.000 seconds (95% CPU, 1874459 Lips)
% 8,378 inferences, 0.001 CPU in 0.001 seconds (100% CPU, 9221480 Lips)
----

Here's a faster version:

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

The lesson to be learned is that there is a performance cost to using CHR to do tasks Prolog could do simply.

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

link:http://chr.informatik.uni-ulm.de/~webchr/[WebCHR]
 is useful for experimenting with CHR, and has some more examples.

At times CHR's single threaded nature can be painful. 
link:https://github.com/fnogatz/CHR-Constraint-Server[Falco Nogatz]
 has created a server that single threads CHR calls.

For http applications, it might be better to start with the 
link:https://github.com/SWI-PrologTeamLudumDare32/LudumDare45[Ludum Dare Team 45 Server]
largely copied from Falco's, but turned into an HTTP server.

The official 
link:https://dtai.cs.kuleuven.be/CHR/[CHR Website] is a resource for both information and tools.

Some of these tools use the optional **name** you can apply to CHR rules:

----
my_rule @ foo <=> bar.
----

Using the CHR debugger
~~~~~~~~~~~~~~~~~~~~~~

The SWI graphical debugger interacts poorly with CHR, displaying the compiled code.

The text based 
link:https://www.swi-prolog.org/pldoc/man?section=debugging[CHR debugger] is fairly straightforward.

I never found the _leashing_ options that useful with the Prolog text debugger, but they are quite useful with the CHR debugger. 

This handy pattern prints out the execution of CHR without stopping.

----
% print out execution without pausing
?- chr_leash(-all).  chr_trace.  query(2,3,N).
----

Simple Tools
~~~~~~~~~~~~

I also find it useful to have some simple tools in my startup.

----

		 /*******************************
		 *     helpful utilities        *
		 *******************************/

% print out the constraint store
ps :-
    current_chr_constraint(Module:Name),
    format('constraint store contains ~w:~w~n', [Module, Name]),
    fail.
ps.

% print out constraint store when you return to top level
ss :- set_prolog_flag(chr_toplevel_show_store, true).

% or don't
noss :- set_prolog_flag(chr_toplevel_show_store, false).
----

An easy way to switch between production and debug (see above in 'CHR Options') is
useful for debug.

Top Level Removes Residual Constraints
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

CHR removes all residual constraints when you return to the top level.
This can be annoying when trying to debug or program at the interactor.

SWI-Prolog has a special toplevel mode just for preventing this:

----
% saner way to do same
?- set_prolog_flag(toplevel_mode, recursive).
----

An alternative to this is Falco Nogatz's
link:https://github.com/fnogatz/CHR-Constraint-Store[CHR Constraint Store]
 that provides a CHR repl.

CHR to JS compiler
~~~~~~~~~~~~~~~~~~

Falco Nogatz has a 
link:https://github.com/fnogatz/CHR.js[CHR to JavaScript compiler]
.

There is a 
link:http://chrjs.net/[website for this project]
.

CHR to SQL compiler
~~~~~~~~~~~~~~~~~~~

A similar compiler exists that 
link:https://github.com/awto/chr2sql[converts CHR to SQL]
.

Conclusion
----------

Here's a truly advanced CHR trick.  CHR is available embedded in many languages.

Since CHR **looks** more like a library than a language, your day job may have less resistance.

Once you're using CHR, well, the camel's got it's nose in the tent.

On to the 
link:final.html[Final Section]



