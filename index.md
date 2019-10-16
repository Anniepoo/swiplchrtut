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

This tutorial is divided into 6 chapters.

. Intro
** Who should do this tutorial?
** Why another tutorial?
** What is CHR good for?
** Intro to CHR
. Basics
** The constraint store
** Arguments
** defining CHR constraints
. Constraint Systems
. Examples and Patterns
** A set of worked examples 
** That can be used as exercises
** And demonstrate common patterns
. Advanced material
** constraint systems
** modes
** types
** advanced syntax
** performance
. Tools
** Debug and usage tools
** References
. Final test

Who Should Do This Tutorial
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This tutorial is for reasonably experienced SWI-Prolog programmers who want to use Tom Freuwerth's _Constraint_Handling_Rules_ system with **SWI-Prolog**.

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

Seek out other tutorials. Tom Schrijvers Slide set from his ICLP presentation is particularly useful.

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

[NOTE]
.Exercise - Punch and Run
=====================================================================
Run the above salt water example.

Make iced tea. Iced tea is made of water, sugar, and tea_bag.
=====================================================================

Conclusion
-----------

CHR is a powerful addition to the logic programmer's toolkit. I hope you'll find it useful.

I hope you've enjoyed this tutorial. If you notice any mistakes, or want to suggest improvements, or just are totally stumped, email annie (at) swi-prolog (dot) org and let me know.

You can often find me as Anniepoo on ##prolog channel on freenode.net IRC.

Thanks to Thom Fruewirth for the CHR library and for answering questions on the CHR list and email. 
Thanks to Alan Baljeu for much patient coaching on the CHR list and for spending a while on video call explaining CHR.
Thanks to Falco Nogatz for yet more explanations, and for the CHR single threaded server.
Thanks to Tom Schrijvers, whose slides from ICLP are a great resource. I've also stolen a few examples in this tutorial from his work. Thanks to Michael Richter, who puzzled out bits of this with me and in particular how the right hand side works. Thanks to Gergö Barany for a pleasant afternoon in Vienna spent puzzling out bits of CHR.




########################################################################################
summative assessment (add more assement in middle)


 *  useful references
 *  https://dtai.cs.kuleuven.be/CHR/files/tutorial_iclp2008.pdf
 *  https://dtai.cs.kuleuven.be/CHR/
 *  https://dtai.cs.kuleuven.be/CHR/webchr.shtml
 *  https://www.swi-prolog.org/pldoc/man?section=chr
 *

========== end of outline area ===========

topics
resolution algorithm
the RHS
Should I add or replace?
CHR or DCG?
Conflict between recognition of different objects.
CHR isnt prolog, it's committed, it doesn't backtrack and you can't bind in head.
failure
pragmas
name
performance
patterns
get_foo pattern
aggregation pattern
negation
backtracking over a constraint removes from store
don't bind in head
don't bind in guard

single threaded
mention falco's server and the LD45 server
making constraint systems
modes
types
tools - all the recursive top level, ss, noss etc tools

============ end of topics, this is the old stuff (useful) ==========

TODO


when prolog encounters a chr constraint as a goal it acts like it's encountered a clp(x)
type constraint - if the constraint is decidable, it succeeds or fails. If it is not
it adds the constraint to the constraint store.

When a variable involved in a constraint is grounded (TODO what about clp(fd), how's it interact
with those constraints?) it checks the guard, if there is one. If so, it executes the body.
if tthe body fails, then whatever grounded the variable fails.

If the body contains chr constraints, they will be treated as in any other prolog code,
so

red, blue <=> yellow.  

when red and blue are in the store, they are


 discard_a, discard_b <=> c   simplification, remove a and b and add c (or whatever the body action is)
 keep_a, keep_b ==> c    propagation  - keep a and b and  add c (or whatever the body action is)
discard_a \ keep_b <=> c   simpagation - discard a  , keep b, add c (or whatever the body action is)

committed choice -
CHR looks down from top to find a rule that fires. Then it fires. So subsequent rules are
ignored

http://voidexception.weebly.com/committed-choice-execution.html

The | is allowed in the body as disjunction to prevent this.



library(chr) is a library included in the standard SWI-Prolog distribution. It makes it easier
to set up constraint systems.

Prolog is typeless. This makes rapidly discovering type errors at runtime critical. Constraint systems
allow this. Additionally, constraint systems are often far faster than hand rolled algorithms.

========
demonstrates that if you backtrack it undoes the changes to the constraint store.
:- use_module(library(chr)).

:- chr_constraint foo/0,taco/0.

init :-
    between(1, 10, _),
    foo,
    fail.
init :- taco.


foo ==> taco.

========

TODO other uses of chr

Boney on chr


[Wednesday 17 April 2019] [11:21:11 PM PDT] <anniepoo> oh, nice demo - make some symbolic ai-ish thing that uses type constraints- like, X is a thing Bob wore, and X is a thing Bob purchased, so 'pants' is OK, but 'a book' is not
[Wednesday 17 April 2019] [11:24:29 PM PDT] <Boney> Yeah.
[Wednesday 17 April 2019] [11:25:26 PM PDT] <anniepoo> the examples that exist are all so simple it's hard to get a sense how to
[Wednesday 17 April 2019] [11:25:27 PM PDT] <Boney> You wear X over Y, and Y = underpants. And such details.
[Wednesday 17 April 2019] [11:25:31 PM PDT] <anniepoo> really use it.
[Wednesday 17 April 2019] [11:25:40 PM PDT] <anniepoo> oh, yes! that's a good idea
[Wednesday 17 April 2019] [11:26:00 PM PDT] <anniepoo> that was actually the very first program I tried to write (beyond family tree and such) in Prolog
[Wednesday 17 April 2019] [11:26:03 PM PDT] <Boney> I don't know if CHR can actually be used for type systems, I think it can, I think I thought of that and also read it somewhere..
[Wednesday 17 April 2019] [11:26:18 PM PDT] <Boney> X is held up by Q,
[Wednesday 17 April 2019] [11:26:24 PM PDT] <Boney> Q goes over your shoulders.
[Wednesday 17 April 2019] [11:26:49 PM PDT] <Boney> many mroe such details.
[Wednesday 17 April 2019] [11:27:34 PM PDT] <anniepoo> as a type system, this would be a dynamic type system.
[Wednesday 17 April 2019] [11:29:53 PM PDT] <anniepoo> but yes, a type system could be something like
[Wednesday 17 April 2019] [11:30:06 PM PDT] <anniepoo> X is a thing Bob might wear.
[Wednesday 17 April 2019] [11:30:18 PM PDT] <anniepoo> then much later on we figure out what X is.
[Wednesday 17 April 2019] [11:30:51 PM PDT] <anniepoo> could be an interactive story the user tells the machine, in a limited domain, and the machine looks for contradictions
[Wednesday 17 April 2019] [11:31:21 PM PDT] <anniepoo> X is a thing bob wore.
[Wednesday 17 April 2019] [11:31:41 PM PDT] <anniepoo> bob wore X under his pants
[Wednesday 17 April 2019] [11:32:38 PM PDT] <anniepoo> and we deduce X is underwear or socks or a tucked in shirt

alanbaljeau on ##prolog on chr
he end of every predicate
[Saturday 20 April 2019] [4:27:32 PM PDT] <alanbaljeu> my other trick is I use CHR a lot, so if i need something available I just make the object and downstream query if it's around.
[Saturday 20 April 2019] [4:27:51 PM PDT] <anniepoo> hey alan, I'm gearing up to write a tutorial on CHR
[Saturday 20 April 2019] [4:27:52 PM PDT] <alanbaljeu> not something i'd recommend if it wasn't already part of your toolbox
[Saturday 20 April 2019] [4:28:10 PM PDT] <anniepoo> but like many of my tutorials, it's motivated by me wanting to learn.
[Saturday 20 April 2019] [4:28:19 PM PDT] <anniepoo> So waht's some cool, small uses of CHR?
[Saturday 20 April 2019] [4:28:36 PM PDT] <nicoabie> what is CHR?
[Saturday 20 April 2019] [4:28:47 PM PDT] <alanbaljeu> Constraint Handling Rules.
[Saturday 20 April 2019] [4:29:07 PM PDT] <alanbaljeu> for example you can very easily write your own CLP variant.
[Saturday 20 April 2019] [4:29:13 PM PDT] <nicoabie> nice name
[Saturday 20 April 2019] [4:29:24 PM PDT] <anniepoo> Constraint Handling Rules - a forward chaining system within swipl . the interface with prolog is constraints, so yah, you can make your own constraint system pretty easy
[Saturday 20 April 2019] [4:29:55 PM PDT] <nicoabie> clp constraint logic programming?
[Saturday 20 April 2019] [4:30:05 PM PDT] <alanbaljeu> yes
[Saturday 20 April 2019] [4:30:07 PM PDT] <anniepoo> that's a set of constraint systems
[Saturday 20 April 2019] [4:30:15 PM PDT] <nicoabie> I didn't get there yet
[Saturday 20 April 2019] [4:30:18 PM PDT] <anniepoo> sorry, clp(fd), etc.
[Saturday 20 April 2019] [4:30:33 PM PDT] <nicoabie> but I think I have used it before, something like glpk?
[Saturday 20 April 2019] [4:30:39 PM PDT] <anniepoo> yeah, nicolas, suggest you leave it for a while
[Saturday 20 April 2019] [4:30:56 PM PDT] <anniepoo> nicolas - there are other states of knowledge ghan knowing or not knowing
[Saturday 20 April 2019] [4:31:09 PM PDT] <anniepoo> knowing = grounded, not knowing = var
[Saturday 20 April 2019] [4:31:23 PM PDT] <anniepoo> that's classic prolog
[Saturday 20 April 2019] [4:31:29 PM PDT] <anniepoo> now, think about my mother's height
[Saturday 20 April 2019] [4:31:46 PM PDT] <nicoabie> var?
[Saturday 20 April 2019] [4:31:52 PM PDT] <anniepoo> you don't know my mom, but you still can say something about her height
[Saturday 20 April 2019] [4:32:03 PM PDT] <anniepoo> unbound variable - not grounded
[Saturday 20 April 2019] [4:32:33 PM PDT] <anniepoo> bound/unbound - the two states of X in prolog
[Saturday 20 April 2019] [4:32:44 PM PDT] <anniepoo> with me?
[Saturday 20 April 2019] [4:32:47 PM PDT] <tjis> I don't trust microsoft's motives
[Saturday 20 April 2019] [4:32:52 PM PDT] <nicoabie> yes
[Saturday 20 April 2019] [4:33:14 PM PDT] <anniepoo> ok, so, you know my mom's height is a number, with a set of units
[Saturday 20 April 2019] [4:33:16 PM PDT] <oni-on-ion> MS isnt exactly a sentient being like a kind of monster
[Saturday 20 April 2019] [4:33:24 PM PDT] <nicoabie> yes
[Saturday 20 April 2019] [4:33:35 PM PDT] <oni-on-ion> not sure how easy it is for many of us to anthropomorphise or generally personify a "company"
[Saturday 20 April 2019] [4:33:35 PM PDT] <anniepoo> so 180cm is reasonable - 14 kg is not. Neither is a list
[Saturday 20 April 2019] [4:33:47 PM PDT] <nicoabie> true that
[Saturday 20 April 2019] [4:33:53 PM PDT] <alanbaljeu> i started using CHR to make a solver for some complex problems. then i started using it as an active state for all kinds of objects. i'm not sure it was a good choice, but it was convenient to do that instead of passing around huge structures of things.
[Saturday 20 April 2019] [4:34:10 PM PDT] <oni-on-ion> one person is an individual. indivisable. a company has many divisions. dot com
[Saturday 20 April 2019] [4:34:14 PM PDT] <anniepoo> you know it's not a negative number, and that my mom's probably not taller than 8 ft or shorter than 3 ft
[Saturday 20 April 2019] [4:34:54 PM PDT] <anniepoo> hmm... what do you mean by 'active state'?
[Saturday 20 April 2019] [4:35:12 PM PDT] <nicoabie> yes, those are the constraints it has to be a pos number between two values
[Saturday 20 April 2019] [4:35:24 PM PDT] <anniepoo> right - so we actually know something about my mom's height
[Saturday 20 April 2019] [4:35:41 PM PDT] <anniepoo> now, I tell you my dad was 5 ft 8, and my mom's shorter than my dad
[Saturday 20 April 2019] [4:35:48 PM PDT] <anniepoo> you can reduce the domain
[Saturday 20 April 2019] [4:36:04 PM PDT] <nicoabie> yeah, it is like glpk
[Saturday 20 April 2019] [4:36:12 PM PDT] <alanbaljeu> :- chr_constraint x/3. ?- x(a, 3, []). <-- this creates an object x in memory that CHR will process.
[Saturday 20 April 2019] [4:36:18 PM PDT] <alanbaljeu> that's the active statet.
[Saturday 20 April 2019] [4:36:33 PM PDT] <anniepoo> ok
[Saturday 20 April 2019] [4:36:47 PM PDT] <anniepoo> oih, yes! ok, CHR is non monotonic
[Saturday 20 April 2019] [4:36:50 PM PDT] <tjis> I don't trust companies with a history of meddling
[Saturday 20 April 2019] [4:36:52 PM PDT] <anniepoo> boing
[Saturday 20 April 2019] [4:37:31 PM PDT] <anniepoo> yeah, nico, so, you can do lots of cool stuff making your code faster, more robust, etc.
[Saturday 20 April 2019] [4:38:22 PM PDT] <oni-on-ion> tjis, strangely i would trust something *more* if its had a rough past. being perfectly good is not possible -- so i am releived when something has already got its "bad stuff" out of the way. i'd hire xcons
[Saturday 20 April 2019] [4:38:24 PM PDT] <nicoabie> I believe I read something in The power of prolog, but I couldn't process it
[Saturday 20 April 2019] [4:39:01 PM PDT] <anniepoo> yeah, you can extend what's known in all sorts of cool ways
[Saturday 20 April 2019] [4:40:03 PM PDT] <anniepoo> you could pput certainties on things, and fail when the certainty falls below a certain value
[Saturday 20 April 2019] [4:40:23 PM PDT] <anniepoo> (hey, dmiles, that's a cool way to improvve reasoning for AGI)
[Saturday 20 April 2019] [4:40:36 PM PDT] <nicoabie> well now I'm very interested in computational semantics, maybe I can put certainties on interpretation of sentences
[Saturday 20 April 2019] [4:40:38 PM PDT] <alanbaljeu> So for example I wrote code that can process [ 2 * X +Y > 3, X > 0, Y < -2, member(X, [5,7,10])] and give a value for X and Y.

.Some TODO Exercise
************

Do something that makes it clearer when CHR is appropriate

*************




