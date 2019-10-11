Tutorial - CHR Constraint Handling Rules
========================================
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
* making programs where things transform into things - 
** A GUI where a button changes appearance when clicked
** A game, where a a spaceship changes appearance after it is hit, fires it's rockets, etc.
** A simulation like a virtual chemistry lab, where adding A and B to the beaker makes C
** An interactive fiction game, where the character moves from room to room
** bottom up recognizers - input characters to words to clauses to sentences to paragraphs.
* writing recognizers - 
** A computer vision system that recognizes simple objects like a chimney, a door, etc. and combines them to make 'house'.
** Expert systems are sometimes mostly recognizers - patient has a cough, fever, trouble swallowing, body aches, they have flu.
* writing expert systems -
** recognizers in expert systems - motor is making noise and input voltage is normal implies bearing failure
** resource utilization - I have an extra gate on this IC and need one for this.
** design constraint systems - 
* implication 
** An AI player for a game with a 'tech tree', where players must build the **tank_factory** to build tanks. Seeing a **tank** means they have the **tank_factory**.
** Reasoning about state. A satellite imagery analysis system sees a combine in a field, and reasons the crop is being harvested, so the local grain elevator is full, so a train will come to pick up the grain.
** When there is a trace on this pcb from a to b, there is an outline of the copper A,B,C,D
** if an account is overdue and the credit score is poor we will not lend money.
* Search
** Flexibly turning normalized data that has to be accessed via joins into single lookup data.

Intro
-----

CHR is an embedded language within Prolog. It focuses on maintaining a set of _constraints_ in a _store_.
You can set up _rules_ so that when a constraint is added to the store, other constraints can appear or disappear from the store and prolog goals can be queried. 

So, in a chemistry simulator, you could add _salt_ and have _salt_ in the store. Then add _tap_water_, and a rule might fire to remove the salt and the water and make _salt_water_.

In CHR you have a _constraint store_, a set of current constraints. You define _CHR constraints_, and then add them to the _constraint_store_ .

CHR myths
~~~~~~~~~

CHR is not a constraint system, despite the word _constraint_ in the name. The word _constraint_ is in the name because the system manipulates _constraints_ in the sense of _predicates_.

CHR is a separate language embedded in Prolog. Because CHR constraint rules look like Prolog, and the right hand side (RHS) acts somewhat similar to Prolog, it is tempting to assume they follow the same rules. They do not. Be warned.

Example
-------

Here's the code to do the above. Don't worry if you don't understand it all now.

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


The Constraint Store
--------------------

As we have seen, the constraint store holds state. We can put things in, and take them out. A practical 

The constraint store can hold multiple copies of a thing. It works like a multiset or bag.

[NOTE]
.Exercise - Multiset Semantics
=====================================================================
Use the above program.

What do you think will happen if you query this? Try it

Query 
?- salt, water, salt.

How about this?

Query 
?- salt, salt, water, water.

=====================================================================


So, if you make salt water twice, you have two salt_waters. 

Arguments
~~~~~~~~~

CHR constraints can have arguments of any Prolog type. 
Suppose we have a large number of beakers, and want to make
salt water in them.

----
:- use_module(library(chr)).

:- chr_constraint salt/1, water/1, salt_water/1.     <1>

salt(N),water(N) <=> salt_water(N).                  <3>

?- salt, water.                                      <4>
salt_water.                                          <5>

?- 
----

<1> This time the chr_constraints have a single  argument. salt(3) means there is salt in beaker 3. Like in Prolog, CHR constraints are defined by their **signature**.
<3> define a CHR rule  - if salt and water are in the store, remove them and put in salt_water.
<4> query that adds salt and water to the store.
<5> CHR prints the contents of the store at the top level by default. The store contains salt_water.

When we call a CHR constraint from prolog that argument unifies with the same position in the CHR constraint.

[WARNING]
.Don't Bind Variables!
=====================================================================
It **does not work** to ground variables on the left side of the operator.
If you need to ground a variable, do it on the right side.
This is probably cryptic, we will explore it later when we cover retreiving
information from the store.
=====================================================================

[NOTE]
.Exercise - arguments
=====================================================================
Use the version of the program with var
What do you think will happen if you query this? Try it

Query 
?- salt(1),water(2), water(1).

How about this?

Query 
?- salt(1),water(2), water(_).

And this?
Query
?- salt(1),water(2), water(X),X=3.

=====================================================================


Defining chr_constraints
~~~~~~~~~~~~~~~~~~~~~~

CHR constraints are defined using the `chr_constraint/1` directive, giving the constraint's signature.

----
:- chr_constraint   foo/1.
----

This must be done **before the first mention in either prolog or CHR code**.

CHR constraints can also be given types and modes, but we'll cover those later.

What makes a good constraint?

**Nouns** make good constraints. `salt` is really 'salt is present'. We could expand our salt water demo considerably, and end up with a chemist's advisor.

A common pattern is to name all the relevant properties of an object. So we might not only say there's `salt`, but `ionic_compound` in some more chemistry oriented version of our salt water program.

**States** make good constraints. In an interactive fiction type game of the `nanisearch` type, `player(bedroom)` is the state of the player being in the bedroom.

States can be properties. `coughing`, meaning the patient is coughing, is a good CHR constraint.

**Verbs** make good constraints. Verbs really say `thing_is_happening`. Usually, adding the verb to the constraint store makes things happen, and then the verb is removed at the end.

Our salt water example has the defect that if we try it in the kitchen, we quickly discover adding salt is not enough. We need to stir as well, particularly if we've added a large quantity.

Let's modify our program to require stirring.

----
:- use_module(library(chr)).

:- chr_constraint salt/0, water/0, salt_water/0, stir/0.

salt,water, stir <=> salt_water.

?- salt, water, stir.
salt_water.

?- 
----

But what happens if we use twice as much salt and water?

----
?- salt, water, salt, water, stir.
salt_water,
salt,
water.

----

We need two stirs. Let's assume this is not what we want and fix it. 

We made `salt_water`, let's also make `stir` on the right

----
:- use_module(library(chr)).

:- chr_constraint salt/0, water/0, salt_water/0, stir/0.

salt,water, stir <=> salt_water, stir.

?- salt, water, salt, water, stir.
salt_water,
salt_water,
stir.

?- 
----

Oh fudge - `stir` remains.

We can fix this with a second rule. This rule will only fire after the first rule can't fire any more. More on this later, but for now, the topmost  rule that can fire, does.

----
:- use_module(library(chr)).

:- chr_constraint salt/0, water/0, salt_water/0, stir/0.

salt,water, stir <=> salt_water, stir.
stir <=> true.

?- salt, water, salt, water, stir.
salt_water,
salt_water.

?- 
----

We keep making salt water until we can't, and then we stop stirring.

[NOTE]
.Exercise - making constraints
=====================================================================
Try all the above examples in this section.

If we try making salt water in a metal bucket, the bucket will rust.
Add a new CHR constraint `rust/0` which is added to the store when `salt_water` is
present.
======================================================================

Basic CHR syntax
----------------

It's time to introduce the CHR syntax in more detail.
There are a few bits
of syntax which will be omitted here and covered later.

We have seen one operator, `<=>`, informally.  There are actually 3 operators. 

<<TODO>> make this pretty

----
name @ discarded <=> guard | body.              <1> Simplification
name @ retained \ discarded <=> guard | body.    <2> Simpagation
name @ retained ==> guard | body.           <3> Propagation
----

All rules have the same structure regardless of their operator.

Name
~~~~

First comes an **optional** _name_, a unique identifier for the rule. This is used by various CHR tools,
but has no effect on the _operation_ of CHR. We will not discuss names further.

Head
~~~~

Next comes the _head_.  The head is a series of CHR constraints which must **all** be present for the rule to fire.
In our first salt water example, both salt and water must be present.

As we've already seen, we can store more than one `salt` in the store. We can have more than one `salt` in the head.

[NOTE]
.Exercise - set semantics
=====================================================================
Let's see if we can change the first program, so `salt` means any non-zero amount of salt, 
and `water` means any non-zero
amount of water, and we only get one `salt_water`.

Hint: you want two salts to become one salt.
======================================================================

This **set semantics** is a frequently useful pattern.

Arguments in constraints called from Prolog unify in the usual way. Repeated argumnts in the head must match,
but **we don't bind variables in the head**. We saw an example of using this earlier, in the multiple beaker
version of salt water.

----
salt(N),water(N) <=> salt_water(N).
----

If there's a `salt(3)` and a `water(4)` then we don't get salt water. The arguments must match.

Operator
~~~~~~~~

We have seen the first type of rule, **Simplification**, denoted by the operator `<=>`. What is on the left hand side
is discarded from the store, and then the right hand side(RHS) is done.

Our 'single stir' example has a defect.  It removes the stir from the store, and then re-adds it. This is inefficient and cumbersome. Not only does it mean there are more database operations, but every time a constraint is added to the store, possibly more rules fire. If we have a rule that stirring makes noise, we get a new noise each time the stir constraint is added (back) into the store.

The **Simpagation** rule type fixes this. It uses the same operator `<=>` as the **Simplification** operator, but has a
`\` backslash in the head. The constraints to the left of the `\` are left in the store, and those to the right removed.

Let's improve our single stir version

----
:- use_module(library(chr)).

:- chr_constraint salt/0, water/0, salt_water/0, stir/0.

stir \ salt, water <=> salt_water.
stir <=> true.

?- salt, water, salt, water, stir.
salt_water,
salt_water.

?- 
----

The third rule type, **Propagation**, retains *all* of the constraints in the head.

[NOTE]
.Exercise - Propagation rule
=====================================================================
Add _noise_ to the constraint store when stirring occurs. Add one noise
for every stir added from outside.

Test your work - does it work even if you don't make salt water?
Do you make many noises if there's lots of salt water being made?
=====================================================================

Guard
~~~~~

Until now we've just been putting a CHR constraint on the right hand side. On one occasion
we used `true`. But the right hand side is much more flexible.

We haven't used a gaurd yet.  A _guard_ is an optional prolog query. If it succeeds, and if the appropriate
constraints are in the store to match the left hand side, then, and only then, will the rule fire.

Suppose we have a bunch of constraints like `quake(Intensity)`. Our instrument is quite sensitive, and we
have many small tremors we can discard. Let's discard all quakes less than 3.0 .

----
guard(Intensity) <=> Intensity < 3.0  | true.
----

Now, a warning. You may NOT bind variables in the guard. A particularly easy
way to do this is to 

[WARN]
.Don't Bind Variables in Guard
=====================================================================
The behavior of binding variables in the guard is undefined, and the
CHR compiler will flag it.
=====================================================================

You can have multiple goals in the guard. They execute left to right in the usual Prolog manner.
There is no backtracking (you can't bind anything anyway). They can share variables with the head,
(as long as they don't bind them) and with each other.

Try to minimize use of guards for performance. Every time the rule might fire, the guard must be executed.

[Exercise]
.Intervals
=====================================================================
Define a constraint `interval/2` whose left argument is the lower end
and whose right argument is the upper end.

Define rules to simplify the constraint store.

For example, interval(1, 5) and interval(4, 9) can become interval(1, 9).
Don't forget interval(1, 5), interval(2,3).

Stretch goal: don't force the user to enter the intervals (low,high).
If they come in (high, low), swap them around.

This exercise is a bit more complex than what you've been doing.
=====================================================================

Body
~~~~

The final part of a CHR rule is the body. This is NOT a prolog rule body. It *is* a comma separated series of **Prolog rules** and **CHR Constraints**. CHR constraints are added to the constraint store as if called from Prolog.
If a Prolog goal fails **the entire attempt to add the original constraint fails**, and the store is returned to it's original state prior to the attempt.

You **ARE** allowed to bind arguments in the body. This becomes important when we look at getting data back from the store.


DONE TO HERE

CHR execution
-------------

<<TODO>> logical view

Making CHR interact with Prolog

Getting 

Examples

Useful patterns

More examples

Advanced CHR syntax

Performance

don't chr for no reason.


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

Radioactive example
we have some isotope americium_241 which decays to neptunium_237 and helium_4.

we init some americium into system,

then add a tick constraint, and write some chr rules so on every tick you get decay, 
with the decay being independent for each atom (so you get the usual decay curve).



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

ay 17 April 2019] [10:50:27 PM PDT] <anniepoo> Hey, does anybody know any good things to build with CHR?
[Wednesday 17 April 2019] [10:51:04 PM PDT] <anniepoo> It's rising to the top of my stack again, I'm just having trouble finding a non-toy project that's suited to it
[Wednesday 17 April 2019] [11:17:36 PM PDT] <Boney> I havn't used CHR, but I can imagine it would be good for implementing a type checker/inference system.
[Wednesday 17 April 2019] [11:18:02 PM PDT] <anniepoo> ooh, type system - ok, good idea
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

Prerequisites
~~~~~~~~~~~~~

Competence in the basics of SWI-Prolog and clp(_).

A level of mathematical sophistication comparable to that of an
upper level standing in a computer science undergrad program.

A working SWI-Prolog installation.

There is no set of additional code for this tutorial TODO. You can cut and paste many of the examples.

What You Can Expect From This Tutorial
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you read through this tutorial and work all the exercises you
can expect to:

 * understand the basic concept of constraint handling rules
 * be able to write programs that find solutions to unknown values TODO fix tthese
 * be able to formulate real world problems using the clp(fd) library predicates
 * be able to select appropriate solutions to constraint system problems.
 * understand how to use clp(fd) in real world SWI-Prolog programs.

Time to Complete
~~~~~~~~~~~~~~~~

Unknown as yet. When the first few people complete this tutorial I'll ask them.

The CLP(FD) Library
-------------------

The clp(fd) library for SWI-Prolog was written by Markus Triska for his PhD thesis. clp(fd) stands for 'Constraint Logic Programming over Finite Domains'.

clp(fd) is one of several constraint solvers available in SWI-Prolog. The others are clp(b), clp(qr) and CHR. clp(b) handles booleans. clp(qr) handles rationals and reals. CHR approaches constraint programming differently, using rewrite rules.

The clp(fd) library can be activated by simply

----------------------------
:- use_module(library(clpfd)).
----------------------------

This installs a compile-time hook that optimizes some clp(fd) constraints, and loads a run-time library.

Variables
---------

We use the term _variable_ rather loosely in computer science.

Replacement Variables
~~~~~~~~~~~~~~~~~~~~~

In imperative languages like Java variables are mutable.
Their values change over time.

In C, a variable like x

---------------------
int x;

... what about here? ...

x = 5;
---------------------

always has a value. Before we assign 5 to it, the value is whatever happened to be in that memory location (a source of delight for security programmers everywhere).

Logic Variables
~~~~~~~~~~~~~~~

Variables in a logic language are different. They are much more like variables or 'unknowns' in high school algebra.

In Prolog a variable
can be bound or unbound. An unbound variable will unify with any
value. A bound variable unifies only with one template.

In effect, we either know, or don't know, the value of the variable.

When we try to unify, we are, effectively, asking, "is what we know
about the answer on the left compatible with what we know about the answer on the right?"
Bob claims to be good friends with the Mayor of Maintown. If Bob's the editor of the Maintown Daily, that's entirely believable. If Bob also claims to have never set foot in Maintown, it's inconsistent.

In Prolog we have only two possibilities, for an atomic result. Know absolutely nothing, or know exactly. Obviously we might have unbound variables within a list or term, but for each variable there are only these two states.

Constrained Variables
~~~~~~~~~~~~~~~~~~~~~

But in the real world we can say more about a variable.

We might not know the exact value, but we know it is one of a set of possible values.

We might know it's value is, say, greater than the value of some other variable, whose value we also don't know. We say the variable is *constrained*.

As we begin to accumulate constraints, we start to be able to
reason about the constraints.  
Suppose we have two variables, X and Y, which are integers.

Now I tell you that X is in the range 0 to 10.

Further I tell you that Y is in range 4 to 8.

And finally that X is more than Y.

You can infer that X is within the narrower range 5 to 10, since the lowest value Y can have is 4, and X must be one more than Y.

Here's how it looks in clp(fd). Don't worry if you don't understand all of this yet

------------------------
:- use_module(library(clpfd)).         <1>

test(X, Y) :-
    X in 0..10,     <2>
	Y in 4..8,      <3>
	X #> Y.         <4>


14 ?- test(X, Y).
X in 5..10,           <5>
Y#=<X+ -1,     <6>
Y in 4..8.
-------------------------

<1> include the clp(fd) library
<2> constrain X to be in 0 through 10 using the 'in' operator
<3> constrain Y to be in 4 through 8 using the 'in' operator
<4> constrain X to be greater than Y using the '#>' operator
<5> X is now constrained to 5 through 10
<6> Y is now constrained to be =< X - 1 (same as X > Y), and in the range 4 through 8.

When we have constrained a variable to a single value, something rather magical happens - we now know the value of the variable, so we can bind the variable. And in clp(fd) this is indeed what happens! Suppose I constrain X to the range 0 to 5, and Y to 4 to 8, and X > Y, then suddenly X is now in bound to 5. ground(X) succeeds, and X is a perfectly ordinary bound variable.

------------------------
:- use_module(library(clpfd)).

test2(X, Y) :-
     X in 0..5,
	 Y in 4..8,
	 X #> Y.

16 ?- test2(X,Y).
X = 5,     <1>
Y = 4.     <2>

-------------------------

<1> X is bound just like normal Prolog
<2> Notice that Y is bound too. Further, it's lost it's original constraints

.punch and run Exercise
************

Type in and try the two examples above. Step into them with the graphic debugger to see the constraints listed.

*************

.backtracking Exercise
************

------------
:- use_module(library(clpfd)).

foo(X) :- X in 1..3 .
foo(X) :- X in 5..7 .
foo(X) :- X in 8..12 .
------------

What will happen if you query foo(X) and get all the solutions by backtracking?
Predict what will happen, and then try it. Was your prediction correct?

*************

Implementation
~~~~~~~~~~~~~~

SWI-Prolog has the ability to add *attributes* to variables. This is additional meta-data attached to the variable. If you're interested, you can read more about attributed variables at link:http://swi-prolog.org/pldoc/man?section=attvar[The SWI-Prolog website].

clp(fd) uses this attribute data to decorate the variables with constraints. Then clp(fd) implements constraint programming
as an expansion of the normal Prolog unification.

.attributes Exercise
************

Extra credit -

Think up another use for attributes on variables besides constraints

*************

Domains
~~~~~~~

The (fd) in clp(fd) stands for 'finite domain'. This domain could have millions of values, but it must be a finite list.

We're only concerned with variables with a finite domain, the *finite domain* of the name. For our purposes that means we want to reason about domains that are sets of integers.

You _do need_ to give variables a domain before you try to label them! If not, you'll get a baffling +ERROR: Arguments are not sufficiently instantiated+ exception.

Any finite list of possibilites can be mapped into a finite subset of the integers.

At first this can seem awkward, but it's really no different than using array indices. A bigger issue can be debugging - lists of integers can be pretty meaningless. Writing some debug code to extract the data and print it can help.

An Example
----------

I'm always annoyed by discussions that blather on, ungrounded in reality. So here's a classic constraint program, with brief descriptions of the parts. You may not understand all the parts yet.

The SEND + MORE = MONEY cryptoarithmetic puzzle is a classic
"hello world" for constraint programming. The puzzle is
to assign the digits 0 thru 9 to letters such that they
spell out SEND MORE MONEY and when read as base 10 numbers
create a true mathematical formula. Leading zeros are not
permitted on numbers.

------------------------
:- use_module(library(clpfd)).         <1>

puzzle([S,E,N,D] + [M,O,R,E] = [M,O,N,E,Y]) :-   <2>
        Vars = [S,E,N,D,M,O,R,Y],     <3>
        Vars ins 0..9,      <4>
        all_different(Vars),        <5>
                  S*1000 + E*100 + N*10 + D +     <6>
                  M*1000 + O*100 + R*10 + E #=
        M*10000 + O*1000 + N*100 + E*10 + Y,
        M #\= 0, S #\= 0,    <7>
		label(Vars).  <8>

9 ?- puzzle(X).
X = ([9, 5, 6, 7]+[1, 0, 8, 5]=[1, 0, 6, 5, 2]) ;  <9>
false.
-------------------------

<1> include the clp(fd) library
<2> note the clp(fd) is not an embedded DSL like pce or quasiquotes. clp(fd) 'rides along' with Prolog, adding semantics.
<3> for convenience make a list of all the variables we'll constrain
<4> restrict each variable in Vars to the range 0..9 inclusive
<5> add the constraint that they must all be different
<6> add the arithmetic constraint that defines the puzzle
<7> M and S start words, and hence cannot be zero
<8> attempt to ground as many variables as possible.
<9> notice that there's no funny attribution outside, it just returns the solution

.FortyTenTen Exercise
************

1) Solve the cryptarithm
FORTY + TEN + TEN = SIXTY
by modifying the program

2) are there more solutions to SEND+MORE=MONEY if you allow leading zeros? Modify the program to find out.

*************

Labeling
--------

Attaching constraints is nice, but ultimately of little use until we can get back to simply grounding the variables. A solution with all the variables ground is called a 'ground solution'.

There are a few minor mechanisms for doing so. If you reduce the domain to a single value, the variable will be ground.

Obviously you can simply ground the variable normally +X = 3+

And the predicate +indomain/1+ successively binds a single variable to it's possible values on backtracking.

But usually we call +label/1+ or +labeling/2+ to find a single ground solution. +label/1+ and +labeling/2+ are nondeterministic, binding a different set of values on each backtracking.

Of course our constraint system may not be powerful enough to express every constraint. If so, we must fall back on the generate and test search strategy familiar to all Prolog programmers. With constraint systems, generate and test becomes

. constrain
. generate (by labeling)
. test

and indeed most clp(fd) programs follow a general pattern of constrain followed by label.

[TIP]
It's a good practice to set up the model and do the labeling and any remaining searching in separate predicates. This makes it easier to inspect the model before labeling is applied.

+labeling/2+ has a wealth of options that affect the variable selection strategy. Since most of the time taken by a clp(fd) program is usually in the labeling, we'll cover these in detail later. For now we'll use +label/1+, which has reasonable defaults.

The options in +labeling/2+ are also used with optimization problems of the 'make the most profit' type.

Simple Constraints
-----------------

clp(fd) provides a basic set of primitive constraints.

Remember that clp(fd) works with integers.

.simple operators
-------------------
X #>= Y
X #=< Y
X #= Y
X #\= Y
X #> Y
X #< Y
-------------------

[TIP]
You can use +#=+ as a declarative version of +is+, in what is otherwise not a clp(fd) program. +X is 3+Y+ requires Y to be ground, while +X #= 3+Y+ works if X is ground and Y is not.

[TIP]
+X #< Y+ is useful to get rid of uninteresting symmetries. For example, if players 1 and 3 are matched in a tournament then there's no point considering the match 3 and 1. Consider this example taken from the SWI-Prolog documentation that finds all the ways to pair up 4 people:

.Removing Symmetry
==================

------------------
?- Vs = [A,B,C,D], Vs ins 1..4,
        all_different(Vs),
        A #< B, C #< D, A #< C,
   findall(pair(A,B)-pair(C,D), label(Vs), Ms).
Ms = [ pair(1, 2)-pair(3, 4),
       pair(1, 3)-pair(2, 4),
       pair(1, 4)-pair(2, 3)].
-------------------

==================


.Strictly Increasing Exercise
************
Write a predicate +increase/1+ that takes a list and constrains it to be strictly increasing.

First record what result you expect from each query below. Then test that your predicate does what you expect.

-------------------
?- increase([1, X, 3]).

?- increase([1, X, 4]).

?- increase([1,X, Y, 4]).

?- increase([1,2]).

?- increase([1,X]).
-------------------

*************

Constraining to a Domain
------------------------

Clp(fd) is concerned with the domains of variables.

The _domain_ of a variable is simply the set of values it can take on.

In clp(fd) every variable must be restricted to a "finite domain" in order to reason about it.

-----------
X in 5..10,   <1>
Y in 1..10,   <2>
Y #> X        <3>
-----------

<1> Set a domain for X - in this case 5 through 10
<2> Set a domain for Y - 1 through 10
<3> Now constraint Y to be more than X.  So we can reason about Y's domain. Y must be one of the values 6,7,8,9, or 10.

In and Ins
~~~~~~~~~~

You can restrict the domains of variables with the +in+ and +ins+ operators.

Both +in+ and +ins+ take a domain on the right side.
A domain is a simple range or a union of simple ranges with the
\/ operator

A simple domain can be a single integer or a pair of bounds connected with double dots. The bounds can be integers, or +inf+ for the least member or +sup+ for the greatest member. Any of these can be a ground variable, like +N=3, X in 1..N+ .

The +\/+ operator
~~~~~~~~~~~~~~~~~

Domains can be unioned together with the +\/+ operator

.Unions
--------------------
1..3 \/ 5..7
--------------------

Here's an example of a more complex domain.

.Complex Domain
=====================

---------------------
V in inf.. -2 \/ 0 \/ 2..16 \/ 18..sup
---------------------

=====================

+in+ constrains a single variable.

+ins+ constrains a list of variables to the same values, the equivilent of maplist over the list with in.

Domain From Data
~~~~~~~~~~~~~~~~

What if your domain data comes from input data? Assemble your domain as a term and use X in MyDomain. Here's an example

.A Domain From Data
========================

------------------------
% Bases is a list of ints. Constrain Var to be within B..B+2 for B
% a member of Bases

two_domains(Bases, Var) :-
	make_two_domains(Bases, Term),
	Var in Term.

make_two_domains([H], H..HT) :-
	HT is H + 2.
make_two_domains([H | T], '\\/'(H .. HT, TDomain)) :-
	HT is H + 2,
	make_two_domains(T, TDomain).

25 ?- two_domains([1, 8, 16], V).
 V in 1..3\/8..10\/16..18 ;
-------------------------

=========================

Arithmetic Operators
--------------------

You can do a lot of constraint work with the arithmetic constraints.

Constraints can take infix arithmetic expressions.

---------------
X + 2 #= Y * 3
---------------

The available operators are

- unary -,
- +,
- -,
- *,
- / (truncated integer division),
- ^ (exponentiation),
- min,
- max,
- mod,
- rem,
- abs

Logical Operators
-----------------

+`#\`+ Negation
~~~~~~~~~~~~~~~

Unary. Inverts the contained constraint.

+`#/\`+ And
~~~~~~~~~~~

Constrains both sides to hold. Useful on left side of a +`#==>`+ operator (below).

+`#\/`+ Or
~~~~~~~~~~

Constrains at least one side to hold. Useful on left side of a +`#==>`+ operator (below).

Reification
-----------

Reification is the process of using constraints to control other constraints. clp(fd) includes two operators for reification.

+`#<==>`+ Equivalency
~~~~~~~~~~~~~~~~~~~~~

Each side may be a boolean variable (a 0 or 1) or a constraint. The constraints are modified so they both hold, or their negations both hold.

+`#==>`+ Implication
~~~~~~~~~~~~~~~~~~~~

If the left side holds, then the right side must hold.
If the left side does not hold, then the right side is ignored.


.Some Reified Constraints
=============================

In a chemical plant there is a reaction vessel. The
temperature in the vessel is constrained to be less
than a certain value, and to be more than another value,
except when in 'startup' mode.

-----------------------------

chem_tank(Temp, Startup) :-
	Temp #< 800,
	#\ Startup #==> Temp #> 300.

-----------------------------

We can define startup mode as lasting for 10 minutes after
some StartTime.

-----------------------------

chem_demo(Temp, TimeNow, StartTime) :-
	chem_tank(Temp, TimeNow - StartTime #< 10).

-----------------------------

Notice that +StartTime+ is passed in *as a constraint* . You can build a predicate
that applies constraints and apply conditions under which they apply.

==============================

.Employment exercise
************
Write a small system that matches people with jobs. The jobs have requirements for education level, weight they can lift, age, distance from home. Be able to record special constraints in the individual's
record - eg. Bob's on parole and can't work more than 20 miles from home. Sally wants a helper, but has had problems with workers hanging around when not working, she wants someone who lives more than 10 miles away.

People have funny situations, so try to make it general.

For this exercise you can make the (unrealistic) assumption that users know Prolog.

*************


Commonly Useful Constraints
---------------------------

We now turn to looking at the more 'convenience' predicates provided in the clp(fd) library. Most establish a large number of simple constraints at the same time.

As well as introducing the convenience predicates, it's also a good way to develop a facility with using constraints.

Here are a few constraints that are useful in a wide variety of constraint situations.

length(List, Length)
~~~~~~~~~~~~~~~~~~~~

This of course is good ol' length. It's useful in the length(-, +) mode
to generate a list of unbound variables.

[TIP]
+length_(Length, List) :- length(List, Length).+ is sometimes handy to make lists of unbound variables in call + 1 situations.
+length(List, 9), maplist(length_(9), List)+ gives a 2D array of
unbound variables, for example.

.length Exercise
************

query
------------
?- length(X, 5).
------------

Now try

------------
?- length(X, Y).
------------

*************

all_different(List)
~~~~~~~~~~~~~~~~~~~

This predicate is used frequently. It constrains the members of List to
different values.

.all_different Example
----------------------
 ?- length(List, 4),List ins 1..4, all_different(List), List = [1,_,3,4].
List = [1, 2, 3, 4].
----------------------

+all_different/1+ is useful in conjunction with the 'pigeonhole principle', which says that if you have N variables each an element of 1..M , and N > M, then there must be two variables with the same value. In the above example, List must always be a permutation of [1, 2, 3, 4] for this reason.

All_distinct works like all_different, but 'tries harder' to detect when all are different.

For example,

------------------------
24 ?- X in 1..2, Y in 1..2, Z in 1..2, all_different([X,Y,Z]).
X in 1..2,
all_different([X, Y, Z]),
Y in 1..2,
Z in 1..2.

25 ?- X in 1..2, Y in 1..2, Z in 1..2, all_distinct([X,Y,Z]).
false.

26 ?-
-------------------------

In the above, all_different fails to detect that there are only two possible values for X, Y, and Z, so two of them must be the same.

all_distinct does more work to analyze the domains of it's variables, and does detect this condition. The cpu cost of using all_distinct is higher, however - so this needs balanced against performance gains from pruning the search tree earlier.

If you are having performance issues it might be worth investigating using one or the other.

tuples_in(+ListOfVariableTuples, +ListOfAcceptedTuples)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It's fairly common to have a list of acceptable combinations of
a few variables. A common situation would be ordering American style
meals, where you order a full meal, whose price includes an entree and
two sides, but you only get one side if you get the pie, etc.
You have to enumerate all the various combinations.

The SWI-Prolog documentation contains this cool demo of selecting trains for a journey.

.Train Journey
==============================

------------------------------
:- use_module(library(clpfd)).

trains([[1,2,0,1], % from station, to station, departs at, arrives at
        [2,3,4,5],
        [2,3,0,1],
        [3,4,5,6],
        [3,4,2,3],
        [3,4,8,9]]).

threepath(A, D, Ps) :-
        Ps = [[A,B,_T0,T1],[B,C,T2,T3],[C,D,T4,_T5]],
        T2 #> T1,
        T4 #> T3,
        trains(Ts),
        tuples_in(Ps, Ts).

?- threepath(1, 4, Ps).
Ps = [[1, 2, 0, 1], [2, 3, 4, 5], [3, 4, 8, 9]].
------------------------------

==============================

Beyond the sheer coolness of this as a path finding method,
notice that we didn't have to do any labeling. There's only
one solution.

If you find yourself with tuples that look like +[[1,2], [2,7], [3,8]...]+ consider using +element/3+ instead.

.tuples_in Exercise
************

no 1. Modify the Train Journey program so it finds routes with any number of trains, not just 3.

no 2. You are asked to produce a table of employees who are eligible for promotion.
You have this list of lists. Each inner list is a record for a single employee. The fields are employee #, last review score, # of safety violations, time in grade, required time for promotion

---------
employees([
   [1, 75, 0, 30, 25],
   [2, 83, 0, 45, 25],
   [3, 90, 1, 45, 50],
   [4, 45, 3, 75, 25],
   [5, 89, 0, 52, 50]
   ]).
---------

To be eligible for promotion an employee must have a last review score of 80 or above, no more than one safety violation, and their time in grade must be more than the required time for promotion.

no 3. The last column, time required for promotion, isn't normalized. Employees are either team members or team leaders. Team members need 25 weeks in grade, team leaders need 50. The cheif architect has decided to abstract these into a second table so these numbers can be changed easily.

------------
employees([
   [1, 75, 0, 30, 1],
   [2, 83, 0, 45, 1],
   [3, 90, 1, 45, 2],
   [4, 45, 3, 75, 1],
   [5, 89, 0, 52, 2]
   ]).

time_in_grade([[1,25], [2,50]]).

------------

Update exercise 2 to use the new data format.

*************

Sudoku
~~~~~~

OK, this isn't really the Sudoku predicate, but +transpose(+Matrix, ?Transpose)+ is useful when
the variables are naturally expressed as a 2D grid.

Represent the grid as a list of lists. Each list is
one row.

Many constraint predicates work on adjacency of elements in a list.
If you need to operate on rows you can just use maplist.
To operate on columns, transpose the matrix and they're now
rows.

[TIP]
No, you don't need to transpose back. The new transposed matrix
shares with the original. Any constraints on it are constraints
on the original.

Here, for example, is a program to solve the "quarreling children" problem.
It's made considerably simpler by only dealing with rows

.Quarreling Children
---------------------

/*
	 much shorter quarreling children

	 16 children are to be seated in a
	 4 x 4 array of chairs.

         the children are 8 girls (numbered 1..8) and
	 8 boys (numbered 9..16).

     1,3,5,8 think boys are ucky
	 9,10,11,14 think girls are gross

	 these pairs are enemies

	 [[1,2], [4,6], [4,7], [4, 9],[9,11], [12, 14], [14,16]]

 */

length_(Length, List) :- length(List, Length).

child_row(X) :- X ins 1..16 .

ww(X) :-
	write(X),
	write('/').

print_row(Row) :-
	maplist(ww, Row),
	nl.

children(Class) :-
	length(Class, 4),
	maplist(length_(4), Class),
	maplist(child_row , Class),
	maplist(row_compatible, Class),
	transpose(Class, TransClass),
	maplist(row_compatible, TransClass),
	flatten(Class, FlatClass),
	all_different(FlatClass),
	maplist(label, Class),
	maplist(print_row, Class).

row_compatible([A,B,C,D]) :-
	compatible(A, B),
	compatible(B, C),
	compatible(C, D).

compatible(A, B) :-
	not_enemy(A, B),
	not_enemy(B, A),
	sex_compatible(A, B),
	sex_compatible(B, A).

not_enemy(A, B) :-
	NotA #\= A #\/ NotB #\= B,
	tuples_in([[NotA, NotB]],
		    [[1,2], [4,6], [4,7], [4, 9],[9,11], [12, 14], [14,16]]).

sex_compatible(A, B) :-
	A in 1\/3\/5\/8 #==> B #=< 8,
	A in  9..11\/14 #==> B #> 8.

---------------------

.transpose Exercise
************

Write a predicate that will be part of a roguelike game.

Your game board is a 2D array (list of lists). Each square contains
one of the following

* 0 - floor
* 1 - walls
* 2 - monster
* 3 - the player

write a predicate, can_move(+Board, -Moves)
Board will be a board as defined above
SafeMoves will be a list of lists, with a 1 where the player can move
and a 0 where they cannot. Enforce these rules:

* The player can move 0,1,2, or 3 spaces, connected horizontally and vertically, not diagonally.
* The player cannot pass through or end on walls
* the player cannot pass through or end adjacent to monsters
* The player cannot move off the board

Sample run (some liberties taken for readability):

.board Example
-----------------
board([
[1,1,1,1,1,1,1,1],
[1,0,2,0,0,0,0,0],
[0,1,0,0,0,2,0,0],
[0,0,1,0,0,0,0,3],
[0,0,0,1,0,0,2,0],
[0,0,0,0,1,0,0,0],
[0,0,0,0,0,1,0,0],
[0,0,0,0,0,0,1,0],
[0,0,0,0,0,0,0,1]
]).

?- board(B), can_move(B, M), writeq(M).
M = [
[0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,1,1],
[0,0,0,0,0,0,0,1],
[0,0,0,0,0,0,0,1],
[0,0,0,0,0,0,2,0],
[0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0]
]
-----------------

Hint: write a predicate to print out M in a more readable format
Extra credit: use the terminal coloring library to make that fancy

************

zcompare(?Order, ?A, ?B)
~~~~~~~~~~~~~~~~~~~~~~~~

zcompare is useful when you need to know the relationship between the domains of two unlabeled variables.

--------
10 ?- X in 0..10, Y in 11..20,zcompare(C, X, Y).
C = (<),
X in 0..10,
Y in 11..20.

11 ?- X in 0..11, Y in 11..20,zcompare(C, X, Y).
X in 0..11,
zcompare(C, X, Y),
freeze(C, clpfd:zcompare_(C, X, Y)),
Y in 11..20.

--------

I'll admit, that second one puzzled me a bit. C, it turns out, is still unbound, since we can't really tell the relationship between X and Y when their domains overlap. But if C becomes bound later, then it will go back and constrain X and Y!

--------
15 ?- X in 0..11, Y in 11..20,zcompare(C, X, Y),C = <, writeln(C).
<
C = (<),
X in 0..11,
X#=<Y+ -1,
zcompare(<, X, Y),
Y in 11..20.
--------

[TIP]
Annie is here, looking like this =8cO trying to imagine debugging code that uses lots of zcompares

.zcompare Exercise
************
create a constraint +same_side_of_line/4+ , whose arguments are dicts of the form

------------
point{x:12.34, y:56.78}
------------

The first two arguments are points on a line, the next two are two test points. The predicate succeds if both test points are on the same side of the line, or both are on the line, the constraint holds.

*************

Resources
----------

clp(fd) is great with resource constrained problems.

Scheduling problems are often a repeated series of resource constraint problems.

Most such problems can be worked with the primitive constraints, but a couple built in clp(fd) constraints are useful.
+sum/3+ and +scalar_product/4+ are fairly straightforward.

The use of +scalar_product/4+ might not be obvious. It's useful for defining relationships where different things have different 'costs' (in time, money, or something else).

.Penny Candy
------------------------

%
%  penny candy example
%  Timmy has 25 cents
%  gumballs cost a penny
%  snickers cost 10 cents
%  toffees are 2 cents
%  licorice costs 5 cents
%
%  what are Timmys alternatives?
%  assume Timmy spends the entire 25 cents
scalar_test(Candy) :-
	Candy = [_Gumball, _Snickers, _Toffee, _Licorice],
	Candy ins 0..sup,
	scalar_product([1, 10, 2, 5], Candy, #=, 25),
	label(Candy).

6 ?- scalar_test([Gumball, Snickers, Toffee, Licorice]).
Gumball = Snickers, Snickers = Toffee, Toffee = 0,
Licorice = 5 ;
Gumball = Snickers, Snickers = 0,
Toffee = 5,
Licorice = 3 ;
Gumball = Snickers, Snickers = 0,
Toffee = 10,
Licorice = 1 ;
Gumball = Toffee, Toffee = 0,
Snickers = 1,
Licorice = 3 ;
Gumball = 0,
Snickers = Licorice, Licorice = 1,
Toffee = 5 ;
...

------------------------


[TIP]
Try the penny candy example without the +Candy ins 0..sup+ line.
What will happen?

Sequences
---------

We often need things to be in a specific sequence.

These constraints help establish sequences.

chain(+List, +Relation)
~~~~~~~~~~~~~~~~~~~~~

Chain establishes the Relation constraint between each
pair of adjacent elements in List.

.Chain Example
--------------

chain_example([A,B,C,D]) :-
	chain([A,B,C,D], #>=).

?- chain_example(X).
X = [_G4676, _G4679, _G4682, _G4685],
_G4676#>=_G4679,
_G4679#>=_G4682,
_G4682#>=_G4685.

---------------

lex_chain(+Lists)
~~~~~~~~~~~~~~~~~

+lex_chain/1+ is more interesting.  It takes a list of lists.

It constrains the inner lists to be 'lexicographically non-decreasing'.
This means they should be sorted by columns starting on the left. This is the normal way we sort text

-------------
andy   
babble <-- below andy because a is less than b
beef   <-- below babble because a is less than e
been   <-- below beef because f is less than n
-------------

Besides the obvious use of sorting text, lex_chain has other sorting uses.

.Hospital Patients
************
Patients in a hospital have a coded set of information as a term.

------------
patient(ID, Name, YearAdmitted, MonthAdmitted, DayAdmitted, HourAdmitted, MinuteAdmitted, Status, Payment)
------------

Status is either 0 (normal) or 1 (emergency).

Payment is either 0 (private), 1 (insurance), or 2 (medicaid).

here's some patients

------------
patient(1, 'Bob Jones', 2014, 10, 1, 4, 55, 0, 2).
patient(2, 'Sally Smith', 2014, 9, 29, 5, 15, 1, 0).
patient(3, 'Ted Overton', 2014, 9, 30, 14, 15, 0, 0).
patient(4, 'Arnold Abouja', 2014, 10, 1, 5, 0, 0, 0).
patient(5, 'Seth Humbolt', 2014, 10, 1, 5, 10, 0, 0).
------------

constrain the patients to be seen acording to these rules:

1. See all emergency cases first, before any normal cases.

2. for patients with same status (emergency/normal), see patients in the order they arrived, except

3. If a private or insurance patient arrived up to an hour after a medicaid patient, see them first.

Hint: use lex_chain

*************

element(?Index, +List, ?Element)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Element is the constraint equivilent of +nth1/3+ .

It constrains an element of a list by it's location in the list. Element must be the Indexth member of List, counting from 1.

.Suzy Flirting Example
===========================================

---------------------------
%
% Suzy wants to flirt with Nathan
% But not when her old boyfriend John is around
%
% Suzy, Nathan, and John all must take classes 1..6
%
% How can Suzy arrange her schedule so she can flirt
% in at least 3 classes?

flirt_constraint(Suzy, Nathan, John, FlirtPeriods) :-
	length(Suzy, 6),
	length(Nathan, 6),
	length(John, 6),
	Suzy ins 1..6,
	Nathan ins 1..6,
	John ins 1..6,
	all_different(Suzy),
	all_different(Nathan),
	all_different(John),
	FlirtPeriods = [A,B,C],
	FlirtPeriods ins 1..6,
	A #< B,    % remove unwanted symmetry
	B #< C,
	flirty_period(A, Suzy, Nathan, John),
	flirty_period(B, Suzy, Nathan, John),
	flirty_period(C, Suzy, Nathan, John),
	label(Suzy),
	label(FlirtPeriods).

flirty_period(Period, Suzy, Nathan, John) :-
	Class in 1..6,
	DiffClass #\= Class,
	element(Period, Suzy, Class),
	element(Period, Nathan, Class),
	element(Period, John, DiffClass).
--------------------

======================================

A very important use of element is in helping to constrain different facts.

For example, suppose we have a group of people, and we've numbered them by the alphabetic order of their names. Now we want to constrain a pair to be likely romantic partners. Being heterosexist and ageist, lets say we want to constrain them to be of opposite sexes and within 10 years difference in age.

This example also shows getting from index numbers to what we finally want - the names. It violates a rule of style for clp(fd) - set up your model in one predicate, label it in another - for pedagogic reasons.

.Romantic Partners
==============================

------------------------------
:- use_module(library(clpfd)).

names([amy,bill,charley,deanna,eric,frieda,george,harley]).
% women are 1, men are 0
genders([1,0,0,1,0,1,0,0]).
ages([22,19,73,65,40,38,25,27]).

% maps compatible names
romance(A, B) :-
    names(Names),
    length(Names, NameLength),
    AIndex in 1..NameLength,
    BIndex in 1..NameLength,
    genders(G),
    element(AIndex, G, AG),
    element(BIndex, G, BG),
    AG #\= BG,
    ages(Ages),
    element(AIndex, Ages, AAge),
    element(BIndex, Ages, BAge),
    AAge #< BAge #==> AAge + 10 #>= BAge,
    AAge #>= BAge #==> BAge + 10 #>= AAge,
    AIndex #< BIndex, % remove unwanted symmetry and reflexiveness
    label([AIndex, BIndex]),
    nth1(AIndex, Names, A),
    nth1(BIndex, Names, B).


7 ?- romance(A,B).
A = amy,
B = bill ;
A = amy,
B = george ;
A = amy,
B = harley ;
A = charley,
B = deanna ;
A = eric,
B = frieda ;
false.

------------------------------

==============================

circuit(+List)
~~~~~~~~~~~~~~

The documentation for this is more confusing than enlightening.

+circuit/1+ takes a list of variables and constrains them to form a 'circuit' - a sequence where each number after the first is Vn+1 = Vn mod L + 1 . Where Vn is an element, Vn+1 is the next element, and L is the length of the list.

So this is 'clock arithmetic'.

.circuit example
-----------------
?- length(Vs, _), circuit(Vs), label(Vs).
Vs = [] ;
Vs = [1] ;
Vs = [2, 1] ;
Vs = [2, 3, 1] ;
Vs = [3, 1, 2] ;
Vs = [2, 3, 4, 1] .
-----------------

automaton
~~~~~~~~~

Automaton is the 900 lb gorilla of sequencing.

Automaton comes from _automata theory_ , the study of abstract machines. +automaton/3+ constrains it's first argument to be an element of a language that can be recognized by a finite acceptor. +automaton/8+ constrains it's first argument to be an element of a language that can be recognized by a pushdown acceptor.

Do what? (Mathematicians avert your eyes, I'm going to explain finite automata while playing fast and loose).

A finite automaton is a bit like a hopscotch game. You have a (finite) set of _states_ and directional arcs between them. Each arc is associated with an input. Here's a typical automaton.

image::images/automatona.png[A Typical Automaton]

Start on the green state. Read the first element in the sequence, and if there's an arc labeled with it, go to that state. If not,  your sequence is not in the language. If there is, repeat in the new state - read the next element, and try to follow the arc. If you are in a blue state what you've read so far is part of the language.

Our language accepts one or more 0's, followed by a 1, then a 2.

The green states are called source states, the blue ones sink states. A state could be both a sink and a source.

And here's how to encode it in SWI-Prolog:

.singlesourceautomaton
========================

-------------------------

single_source_automaton(Vs) :-
	automaton(Vs, [source(a), sink(d)],
		  [arc(a,0,b),
		   arc(b,0,b),
		   arc(b,1,c),
		   arc(c,2,d)
		   ]).

demo_single_source(Vs) :-
	length(Vs, 4),
	single_source_automaton(Vs),
	label(Vs).

-------------------------

========================


Here's a variation on our first automaton that accpets the same as the first automaton, but allows you to add 10 to all the numbers, so it accepts sequences like 10,10,10,10,11,12.

image::images/automaton.png[Variation that also accepts sequences with each 10+ first]

.multisourceautomaton
=====================

----------------------

multi_source_automaton(Vs) :-
	automaton(Vs, [source(a),source(e), sink(d), sink(h)],
		  [arc(a,0,b),
		   arc(b,0,b),
		   arc(b,1,c),
		   arc(c,2,d),
		   arc(e,10,f),
		   arc(f,10,f),
		   arc(f,11,g),
		   arc(g,12,h)]).

demo_len(Vs) :-
	length(Vs, 4),
	multi_source_automaton(Vs),
	label(Vs).

-----------------------

======================

automaton/3 example - certain employees can't work at night, others on weekends, we encode this with an automaton that reads a list of who'se on shift.
Maybe enough with the employee examples - how about valid messages?

Automaton/8  - on hold til I understand it

example for counters - extend the employee example so employees get a max and min number of shifts


Scheduling
---------

Scheduling is something we do every day. Bob can only meet from 2 to 3, you need a couple uninterrupted hours to think about the data base, mom's coming by for lunch. It's also a major economic constraint - the factory can make tractor parts from march to july but has to convert to making beanie babies for christmas.

serialized
~~~~~~~~~~

Serialized expresses the common constraint of not being in two places at once. It takes two lists of the same length, the first is start times and the second durations. It constrains them that intervals defined by the starts and durations don't overlap. Notice that the starts don't have to be in order - that is, serialized([4,0,2], [1,1,1]) succeeds.

.dailyschedule
=====================

----------------------
:- use_module(library(pairs)).

my_schedule_today(Starts, Durations) :-
  % unordered list of stuff to do today
  % in a real problem we'd probably use minutes, these are hours in 24 hour format
    Starts = [PrepForSmith, MeetWithSmith, _WriteDBInstaller, Lunch, _CallAlice, _ReadDocs],
  % and how long they'll take
    Durations = [2, 1, 2, 1, 1, 1],
  % make all of them start in 9am to 5pm
    Starts ins 9..17,
  % and make sure lunch happens at noon or 1pm
    Lunch in 12 \/ 13,
  % Meeting with Smith is planned for 1pm
    MeetWithSmith #= 13,
  % have to do the prep before the meeting
	PrepForSmith #< MeetWithSmith,
  % enforce being serialized
    serialized(Starts, Durations).

demo_my_schedule(Starts, Durations) :-
	my_schedule_today(Starts, Durations),
	append(Starts, Durations, Vars),
	label(Vars),
	pairs_keys_values(NameDurs ,
       ['Prep for Smith', 'Meet With Smith', 'Write DB Installer', 'Lunch', 'Call Alice', 'Read Flubbercalc Docs'], Durations),
	pairs_keys_values(Pairs, Starts, NameDurs),
	keysort(Pairs, Sorted),
	pairs_keys_values(Sorted, SortStarts, SortNameDurs),
	print_sched(SortStarts, SortNameDurs).

print_sched([], _).
print_sched([Start | ST], [Name-Duration | T]) :-
	format('~w: ~w  (~w hr)~n', [Start, Name, Duration]),
	print_sched(ST, T).

8 ?- demo_my_schedule(Starts, Durations).
9: Prep for Smith  (2 hr)
11: Call Alice  (1 hr)
12: Lunch  (1 hr)
13: Meet With Smith  (1 hr)
14: Write DB Installer  (2 hr)
16: Read Flubbercalc Docs  (1 hr)
Starts = [9, 13, 14, 12, 11, 16],
Durations = [2, 1, 2, 1, 1, 1] ;
9: Prep for Smith  (2 hr)
11: Call Alice  (1 hr)
12: Lunch  (1 hr)
13: Meet With Smith  (1 hr)
14: Write DB Installer  (2 hr)
17: Read Flubbercalc Docs  (1 hr)
Starts = [9, 13, 14, 12, 11, 17],
Durations = [2, 1, 2, 1, 1, 1]

-----------------------

======================

global_cardinality/3
~~~~~~~~~~~~~~~~~~~~

+global_cardinality/3+ counts the number of occurances of each value in it's first argument.

Here's an example. Suppose we have a list of employees for the shifts in a 24/7 convenience store.

--------------
shifts([1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,4,5,6,4,5,3]).
--------------

we want to find out how many shifts each employee gets.

--------------
shifts([1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,4,5,6,4,5,3]).

count_shifts(Counts) :-
     shifts(Shifts),
     bagof(X, between(1,6,X), Keys),
     length(Values, 6),
     pairs_keys_values(Counts, Keys, Values),
     global_cardinality(Shifts, Counts, []).
--------------

Remember, we're in constraint land, so this works even if you don't know
shifts 'ahead of time'

---------------
post_shifts(Counts) :-
     length(UnknownShifts, 21),
     bagof(X, between(1,6,X), Keys),
     length(Values, 6),
     pairs_keys_values(Counts, Keys, Values),
     global_cardinality(UnknownShifts, Counts, []),
     % at this point I have Counts and UnknownShifts constrained,
     % though I don't know UnknownShifts
     shifts(UnknownShifts). % now it's bound
----------------

+global_cardinality/3+ has two options

* +consistency(value)+  -  presents a 'weaker' consistency. I don't know what this is, if you find out let me know
* +cost(Cost, Matrix)+ - given a matrix of values for key i in position j, constrain the total cost to Cost

.global_cardinality Exercise
***************

Design an exercise that covers the +cost/2+ option of global_cardinality. Include a solution.

***************

+cumulative/2+ and +cumulative/1+
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+cumulative/2+ takes a list of _tasks_ and constrains them to not exceed a limit at any time during the tasks.

Tasks can be specified flexibly, by any combination of two of start, duration, and end. Each task also has an amount of resources it uses, and a task identifier.

+cumulative/1+ just defaults the limit to 1.

.mechanics
===============

---------------------------
%
%  We have 3 mechanics, and a number of repair jobs to do
%  Transmission jobs take two mechanics
%
%  Find a set of start times for the jobs during a 10 hour day
%
task_names([
    transmission, brake_job_1, brake_job_2, diagnosis_1,
    diagnosis_2, fuel_injection]).

tasks([
    task(S1,3,_,2,1),
    task(S2,1,_,1,2),
    task(S3,1,_,1,3),
    task(S4,3,_,1,4),
    task(S5,3,_,1,5),
    task(S6,2,_,1,6)], [S1, S2, S3, S4, S5, S6]).

find_task_starts(S) :-
	length(S, 6),
	S ins 0..9,
	tasks(Tasks, S),
	cumulative(Tasks, [limit(3)]),
	label(S).

2 ?- find_task_starts(S).
S = [0, 0, 1, 2, 3, 3] ;
S = [0, 0, 1, 2, 3, 4] ;
S = [0, 0, 1, 2, 3, 5] ;
S = [0, 0, 1, 2, 3, 6] ;
S = [0, 0, 1, 2, 3, 7] .
---------------------------

===============

.cumulative Exercise
**********************

1) It looks like the mechanics aren't very busy. Add a few more tasks.

2) Mechanic 3 only works the first 3 hours of the day. Modify the example to take this into account.

**********************

Optimization
------------

We frequently need not only to satisfy a set of constraints, but to find the optimal solution. Problems like "Which of these products should this factory make to maximize profit?" and "Using 3 mechanics, how fast can we repair all these cars?" are optimization problems.

+label/1+ has an arity two version with a slightly different name, +labeling/2+.
The options are the first argument. The options are important for optimization problems, and for increasing the efficiency of the labeling process.

The two we're concerned with right now are +max/1+ and +min/1+ . Adding one of them to the options causes the solutions to be presented in order, with the variable in the leftmost max or min sorted first, then proceeding down the list.
max terms sort decreasing order, min terms sort increasing order.

.inorder
-------------------------------------------------------------
7 ?- [X,Y] ins 1..3, labeling([max(X), min(Y)], [X,Y]).
X = 3,
Y = 1 ;
X = 3,
Y = 2 ;
X = Y, Y = 3 ;
X = 2,
Y = 1 ;
X = Y, Y = 2 ;
X = 2,
Y = 3 ;
X = Y, Y = 1 ;
X = 1,
Y = 2 ;
X = 1,
Y = 3 ;
false.
-------------------------------------------------------------

Making Labeling Fast
--------------------

Internally, +labeling/2+ attempts to assign values to variables one at a time. Each time a value is chosen, a subtree of possibilities is removed. You want to make those subtrees as large as possible.

How To Label Efficiently
~~~~~~~~~~~~~~~~~~~~~~~~

Start by ordering your variables for labeling. Place the variables that will reduce the search tree by the largest value first. Knowing which these are depends on knowledge of the domain, but you can accumulate data in use if necessary.

Often, within a variable you have latitude how to assign labels. Suppose you're working on a medical treatment recommending system. Here's the items:

* treat for radiation exposure
* prescribe antibiotics
* provide hyperbaric repressurization
* intubate to provide airway
* prescribe painkillers
* prescribe anti-cobra venom
* prescribe blood pressure regulators

Common sense is that, hopefully, radiation exposure will be less common than sniffles, so we'll be matching 'prescribe antibiotics' more often than 'treat for radiation exposure'.  So examining items in this order is likely to be faster:

* prescribe painkillers
* intubate to provide airway
* prescribe blood pressure regulators
* prescribe antibiotics
* provide hyperbaric repressurization
* prescribe anti-cobra venom
* treat for radiation exposure

In Prolog this will look something like

.betterOrder
----------------
treatments([painkillers,
			intubate,
			bp_regulators,
			antibiotics,
			hyperbaric_chamber,
			anti_cobra_venom,
			radiation_exposure
			]).
-----------------


settings
~~~~~~~~

There are 3 settings in the first argument of +labeling/2+ ; variable selection strategy, value order, and branching strategy. Choose one value for each setting (or use the defaults).

At most one option of each category can be specified, and an option must not occur repeatedly.

To some extent, picking the best settings is a matter of experiment. If you get desperate, there's an undocumented +random_value(Seed)+ (props to https://github.com/adbenitez[adbenitez] for pointing
this out) or +random_variable+ .

Parts of this section are copied from the documentation.


variable selection strategy
^^^^^^^^^^^^^^^^^^^^^^^^^^^

The variable selection strategy lets you specify which variable of Vars is labeled next and is one of:

* +leftmost+ - Label the variables in the order they occur in Vars. This is the default.

* +ff+  First fail. Label the leftmost variable with smallest domain next, in order to detect infeasibility early. This is often a good strategy when there are small domains for the subsequent variables when a first variable is chosen.

* +ffc+ Of the variables with smallest domains, the leftmost one participating in most constraints is labeled next. Applying a constraint has to remove a subtree, so this can be a good strategy.

* +min+  Label the leftmost variable whose lower bound is the lowest next. note that this is +min/0+, different than +min/1+, which determines solution order and is discussed in the previous section above. This is a good tactic if you're trying to minimize some global value that is likely to be lower if various variables are (e.g. a minimum cost solution).

* +max+  Label the leftmost variable whose upper bound is the highest next. This too is different than +max/1+.  And the advice for +min+ applies to +max+ when trying to maximize a global value.

value order
^^^^^^^^^^^

The value order is one of:

* +up+ Try the elements of the chosen variable's domain in ascending order. This is the default.

* +down+ Try the domain elements in descending order.

Obviously, if you've got an assymmetric distribution, like we demonstraed in how to label efficiently above, try elements in most common first order.

branching strategy
^^^^^^^^^^^^^^^^^^

The branching strategy is one of:

* +step+ For each variable X, a choice is made between X = V and X #\= V, where V is determined by the value ordering options. This is the default.

* +enum+  For each variable X, a choice is made between X = V_1, X = V_2 etc., for all values V_i of the domain of X. The order is determined by the value ordering options.

* +bisect+ For each variable X, a choice is made between X #=< M and X #> M, where M is the midpoint of the domain of X. Choose this option if many variables are selections among a range of integers, a 'value', rather than one among a set of enumerated values (e.g. percentages, vs a=0, b=1, c=2)

Debugging and Testing
---------------------

Isolating the portion of the program that sets up the constraints allows you to
see the constrained domains in the top level.

Here are some predicates that are useful for debugging and for interfacing clp(fd) with the rest of the Prolog environment.

+fd_var(+Var)+ True iff Var is a CLP(FD) variable.

+fd_inf(+Var, -Inf)+  Inf is the infimum of the current domain of Var.

+fd_sup(+Var, -Sup)+ Sup is the supremum of the current domain of Var.

+fd_size(+Var, -Size)+ Determine the size of a variable's domain. Size is the number of elements of the current domain of Var, or the atom sup if the domain is unbounded.

+fd_dom(+Var, -Dom)+  Dom is the current domain of Var. This predicate is useful if you want to reason about domains. If you just want to display domains for debugging, use the graphic debugger or toplevel.

+indomain/1+ binds it's argument to members of it's domain on backtracking.

For example, to implement a custom labeling strategy, you may need to inspect the current domain of a finite domain variable. With the following code, you can convert a finite domain to a list of integers:

.domainAsIntegers
=================

-----------------
dom_integers(D, Is) :- phrase(dom_integers_(D), Is).

dom_integers_(I)      --> { integer(I) }, [I].
dom_integers_(L..U)   --> { numlist(L, U, Is) }, Is.
dom_integers_(D1\/D2) --> dom_integers_(D1), dom_integers_(D2).
Example:

?- X in 1..5, X #\= 4, fd_dom(X, D), dom_integers(D, Is).
D = 1..3\/5,
Is = [1,2,3,5],
X in 1..3\/5.
-----------------

=================

Consider clp(fd) for writing mocks.

+call_residue_vars/2+ calls a predicate, then reports all unresolved constraints or frozen variables. Since usually a proper program resolves all it's variables, this is useful for finding things not resolved.

Beware that it may be buggy.

Applying Constraints in Stages
------------------------------

Being able to 'hang on' to a system of of constrained variables, apply more, and then back up and remove the later constraints is useful.

For example, suppose you have a constraint based drawing program - the sort of program where you draw pieces, and if one is nearly aligned with the other, the program ''realizes'' you want them vertically or horizontally aligned. If you then drag one of the blocks, they all move to maintain the constraint.

To resolve the actual position of the blocks, you're going to have to constrain one of them to the position it was dragged to, causing everything to constrain to single values. But now you've lost all your constraints!

There are two solutions to this. The first, backtracking, can work in many situations, but of course enforces a certain structure on the program. So this is more appropriate for situations like a mixed search-constraint strategy, where you set up some constraints, then search, adding different constraints, and backtracking if the problem becomes overconstrained.

The second solution is +copy_term/2+ .

+copy_term/2+ copies a term (often a list of domain variables) and makes a new term with the same _shape_ but with new variables. The copy includes the attributes (and hence the constraints).

By the same _shape_, we mean:

* atomic items match identical items
* variables match variables
* lists match lists of the same length whose items are the same shape.
* compound terms match compound terms of the same functor and arity, whose arguments are the same shape.

.copyTerm
============

------------
10 ?- copy_term(foo([bar, A, meep(B, [1,2])], 3), Copy).
Copy = foo([bar, _G2450, meep(_G2456, [1, 2])], 3).
------------

============

So what happens with constraints?

.copyTermAttributes
============

-------------
3 ?- X in 0..10, Y in 0..5, X #< Y, copy_term(foo(X,Y), foo(XA, YA)), YA = 3.
YA = 3,
X in 0..4,
X#=<Y+ -1,
Y in 1..5,
XA in 0..2.
-------------

============

Copying half of the constrained variables is generally not a great idea, you'll end up with links outside your constraint system. Not strictly forbidden, but do it only deliberately, since usually you want the constraint system in a 'stable' place you can come back to.

.copyTermHalfAttributes
============

-------------

5 ?- X in 0..10, Y in 0..5, X #< Y, copy_term([Y], [YA]), YA = 3.
YA = 3,
X in 0..4,
X#=<Y+ -1,
Y in 1..5.

--------------

=============

.constrainedGraphics Exercise
********************************

Write a small graphics program, using whatever graphics environment you're comfortable with, that draws a set of dots on the screen. If you move one of the dots to be aligned with another, vertically or horizontally, within a few pixels, then the two should be linked, and move together. Holding down shift always just moves one dot.

Extra Credit
Expand your program into a small diagram editor.

(Note, you're near the end of the tutorial, you might want to read to the end at the same time you're working this problem).

Implement a constraint shell decision support system. This should allow the user to define constraints one at a time using some simple command language (not programming) and see the domains of variables. The user should be able to remove any of the constraints they have entered. Sample run

--------------
?-decision_support.
Decision Support (type h for help)
ds=h
l = list all variables and constraints
<variable name> in <integer> thru <integer> = define new variable
<variable name> <operator> <integer or variable name> = define new constraint
r <number>  = retract constraint <number>
q = quit

ds=a in 1 thru 10
a in 1..10
ds=b in 1 thru 10
b in 1..10
ds=a < b
#1: a < b
ds=b < 5
#2: b < 5
ds=l
variables   
a in 1..3
b in 1..4
constraints
#1: a < b
#2: b < 5
ds=r 2
retracting #2: b < 5
ds=b < 8
#3: b < 8
ds=l
variables
a in 1..6
b in 1..7
constraints
#1: a < b
#3: b < 8
ds=q
?-

--------------

********************************


Making Your Own Constraints
---------------------------

First, you often don't need to make a custom constraint, you need a custom convenience predicate. If you need 'sort of near to', which constrains X and Y to be N to M spaces from each other, then you can write a normal predicate that applies all the constraints. This is  what you've been doing all along in this tutorial.

.sortOfNearTo
=================

-----------------
sort_of_near_to(X, Y, N, M) :-
	X #> Y #==> X - N #>= Y  #/\  X - M - 1 #< Y,
	X #=< Y #==> Y - N #>= X #/\ Y - M - 1 #< X.
-----------------

=================

If you do actually need to write a constraint, here's how.

First, you'll need some setup

.ownConstraintSetup
====================

--------------------
:- use_module(library(clpfd)).

:- multifile clpfd:run_propagator/2.
--------------------

====================

then you need to define the predicate the user uses to apply your constraint.

There are three steps to complete in this predicate.

1. Convert the constraint to an internal form, the 'propagator', by calling +clpfd:make_propagator/2+.
2. Attach each variable to the propagator by calling +clpfd:init_propagator/2+
3. Trigger the propagator so the initial state is set, we fail if we're overconstrained, and the initial domain is set, using +clpfd:trigger_once/1+ .


.evenPred
====================

--------------------
% constrain X to be even
even(X) :-
        clpfd:make_propagator(even(X), Prop),  <1>
        clpfd:init_propagator(X, Prop),  <2>
        clpfd:trigger_once(Prop). <3>
--------------------

====================

<1> Convert the constraint into an internal form +Prop+ , the _propagator_ .
<2> Attach each variable (only X here) to the internal propagator.
<3> Trigger the Propagator once to add the initial state to the system.

Finally, you need to define the conditional check that's called when the constraint is evaluated. This requires adding a clause to +clpfd:run_propagator/2+ .

The first argument of +clpfd:run_propagator/2+ must match the declaration in +make_propagator/2+.

The second argument is a mutable state maintained by clp(fd). It can be used to 'kill' a constraint if it need no longer be triggered, if the constraint has failed or the variable is bound to a valid value, or if it's domain is now reduced to only values that meet the constraint.

.evenCallback
====================

--------------------
% constrain X to be even
clpfd:run_propagator(even(X), MState) :-
        (   integer(X) -> clpfd:kill(MState), 0 is X mod 2
        ;   true
        ).
--------------------

====================

And now we can use our new constraint.

.newconstraint
====================

--------------------
[debug] 26 ?- even(1).
false.

[debug] 27 ?- even(2).
true.

[debug] 28 ?- even(X),X = 2.
X = 2.

[debug] 29 ?- even(X),X = 3.
false.
--------------------

====================

Notice that run_propagator is nondeterministic. It can fail (signaling that there is no solution for X). For example, even(X),X = 3.

.exercise Try even
*************************

Put a spy point on +clpfd:run_propagator/2+ and query even(X), X = 3.

How many times is run_propagator called?

*************************

[TIP]
Fun fact - the Julian library uses clp(fd) extensively.

Conclusion
-----------

Remember, constraint programming is something you should consider any time you have several variables and need to solve for a compatible solution among all of them.

SWI-Prolog ships with a number of constraint libraries. If you're truly inspired, make a tutorial similar to this one for clp(b) or clp(qr) or CHR. If you're inspired to do this, drop me a line, as we're trying to get a nice library of these tutorials together, with a common format so we can automate parts of the process downstream.

I hope you've enjoyed this tutorial. If you notice any mistakes, or want to suggest improvements, or just are totally stumped, email annie66us (at) yahoo_(dotcom) and let me know.

You can often find me as Anniepoo on ##prolog channel on freenode.net IRC.

Thanks to Markus Triska for the clp(fd) library. Hoot Markus!  Thanks to Alan Baljeu for helping with some confusing points. Thanks to Michael Richter for some of the software pipeline setup that produces these tutorials.
