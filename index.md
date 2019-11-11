Tutorial - CHR Constraint Handling Rules - Intro
================================================
Anne Ogborn <annie66us@yahoo.com>
:Author Initials: AO
:toc2:
:icons:
:numbered:
:website: http://www.pathwayslms.com/swipltuts/
:theme: pathways

link:/swipltuts/index.html[Up to All Tutorials]


About This Tutorial
-------------------

This tutorial is divided into 5 chapters.

. Intro
.. Who should do this tutorial?
.. Why another tutorial?
.. What is CHR good for?
.. Intro to CHR
. Basics
.. The constraint store
.. Arguments
.. defining CHR constraints
.. CHR basic syntax
.. Making CHR interact with Prolog
.. Threads
.. Getting
.. Helpful Utilities
. Examples and Patterns
.. A set of worked examples 
.. That can be used as exercises
.. And demonstrate common patterns
. Constraint Systems
.. Review
.. A Simple Constraint System
. Advanced material
.. advanced syntax
.. modes
.. types
.. performance
. Conclusion and Final test

Who Should Do This Tutorial
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This tutorial is for reasonably experienced SWI-Prolog programmers who want to use Tom Freuwerth's _Constraint_Handling_Rules_ system with **SWI-Prolog**. 

Some parts of the material, notably the chapter on constraint systems, depend on a knowledge of constraint
programming. 

If you want to understand making constraint systems, at least read the intro chapter of my [clp(fd) tutorial](/swipltuts/clpfd/clpfd.html), the Prolog clpfd library notes,
and the entries for attributed variables in the SWI-Prolog manual.

The emphasis of this tutorial is *not* on the theory. The author
is not a mathematician, just a long suffering Prolog programmer who
wants to be able to use CHR.

This tutorial should give you a grasp of the fundamentals of CHR. In particular, you
should come away with a good grasp of how to use CHR for real life tasks.

Why another tutorial?
~~~~~~~~~~~~~~~~~~~~~

While there is a fair amount of CHR material available, very little of it explains
how CHR interacts with Prolog. I was motivated to write this by the dearth of good
material helping me understand how the two languages interact.

Additionally, the available examples of CHR tend to be short academic toys. Such are rarely useful
for finding a useful set of patterns for using CHR.

What Is CHR good for?
~~~~~~~~~~~~~~~~~~~~~

CHR started life as a way to set up constraint systems. This is why the word **constraint** is in the name.
While writing constraint systems is an application, it's not the only one.

* writing constraint systems
** When constraint B subsumes constraint A, discard A for better performance
** When there is a route from power to ground, we have a short
** Build a constraint system like clp(fd)
** Build a type system
** Additionally, constraint systems are often far faster than hand rolled algorithms.
* making programs where things transform into things - 
** A GUI where a button changes appearance when clicked
** A game where a a spaceship changes appearance after it is hit, fires it's rockets, etc.
** A simulation like a virtual chemistry lab, where adding A and B to the beaker makes C
** An interactive fiction game, where the character moves from room to room
** bottom up recognizers - input characters to words to clauses to sentences to paragraphs.
** A web application where POST requests modify the store, subject to complex constraints.
* writing recognizers - 
** A traditional bottom up parser
** A computer vision system that recognizes simple objects like a chimney, a door, etc. and combines them to make 'house'.
** Expert systems are sometimes mostly recognizers - patient has a cough, fever, trouble swallowing, body aches, they have flu.
* writing expert systems -
** recognizers in expert systems - motor is making noise and input voltage is normal implies bearing failure
** resource utilization - I have an extra gate on this IC and need one for this.
** "design constraint" systems - like the checker tool before submitting a PCB design
* implication 
** An AI player for a game with a 'tech tree', where players must build the **tank_factory** to build tanks. Seeing a **tank** means they have the **tank_factory**.
** Reasoning about state. A satellite imagery analysis system sees a combine in a field, and reasons the crop is being harvested, so the local grain elevator is full, so a train will come to pick up the grain.
** When there is a trace on this pcb from a to b, there is an outline of the copper A,B,C,D
** if an account is overdue and the credit score is poor we will not lend money.
* Search
** Flexibly turning normalized data that has to be accessed via joins into single lookup data.
* Dynamic type systems. 
** True type systems
** constraints as a substitute for 'real' type systems.

What can I come away with?
~~~~~~~~~~~~~~~~~~~~~~~~~~

If you study each chapter, pass the quizzes, and do the exercises, you should 
be capable of writing CHR at the conclusion.

How Should I study this?
~~~~~~~~~~~~~~~~~~~~~~~~

Definitely do **not** linearly read the material from start to end without exploring.
In particular, check out the [Examples](examples.html) and [Tools](tools.html) sections.

You can ignore the "advanced" material until you've got a solid grounding with the basics.

Read the material. When examples are presented, try them out in SWI-Prolog. Do the exercises.
When you have questions, try to answer them by trying experiments at the interactor or with a simple
program. That's how I learned this material, largely.

[Clone the repo](https://github.com/Anniepoo/swiplchrtut) for this tutorial. Some of the examples are available in the **examples** directory.

Seek out other tutorials. [Tom Schrijvers Slide set from his ICLP presentation](https://dtai.cs.kuleuven.be/CHR/files/tutorial_iclp2008.pdf) is particularly useful.

As you start to see what you can do with CHR, develop a goal. Learning is more fun when it's for a specific purpose.

Don't panic if you don't get it at first. The syntax of CHR is fairly simple, but there are lots of small 
gotchas in the actual use.

CHR myths
~~~~~~~~~

CHR is not a constraint system, despite the word _constraint_ in the name. The word _constraint_ is in the name because the system manipulates _constraints_ in the sense of _predicates_.

CHR is a separate language embedded in Prolog. Because CHR constraint rules look like Prolog, and the right hand side (RHS) acts somewhat similar to Prolog, it is tempting to assume they follow the same rules. They do not. Be warned.

Intro
-----

CHR is an embedded language within Prolog. library(chr) is a library included in the standard SWI-Prolog distribution.

CHR focuses on maintaining a set of _constraints_ in a _constraint store_.
You can set up _rules_ so that when a constraint is added to the store, other constraints can appear or disappear from the store and prolog goals can be queried. 

So, in a chemistry simulator, you could add _salt_ and have _salt_ in the store. Then add _tap_water_, and a rule might fire to remove the salt and the water and make _salt_water_.

Example
-------

Here's the code to do the above salt water example. Don't worry if you don't understand it all now.

----
:- use_module(library(chr)).                         <1>

:- chr_constraint salt/0, water/0, salt_water/0.     <2>

salt,water <=> salt_water.                           <3>

?- salt, water.                                      <4>
salt_water.                                          <5>

?- 
----

<1> use_module CHR
<2> define three _chr_constraints_, objects to put in the constraint store.
<3> define a CHR rule  - if salt and water are in the store, remove them and put in salt_water.
<4> query that adds salt and water to the store.
<5> CHR prints the contents of the store at the top level by default. The store contains salt_water.

[EXERCISE]
.Exercise - Punch and Run
=====================================================================
Run the above salt water example.

Make tea. Tea is made of water, sugar, and tea_bag.
=====================================================================


Conclusion
----------

With some preliminaries out of the way, we're ready to dive into [The Basics of CHR](basics.html)

