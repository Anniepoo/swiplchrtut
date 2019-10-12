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
** Build a constraint system like clp(fd)
** Build a type system
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

salt(N),water(N) <=> salt_water(N).                  <2>

?- salt, water.                                      <3>
salt_water.                                          <4>

?- 
----

<1> This time the chr_constraints have a single  argument. salt(3) means there is salt in beaker 3. Like in Prolog, CHR constraints are defined by their **signature**.
<2> define a CHR rule  - if salt and water are in the store, remove them and put in salt_water.
<3> query that adds salt and water to the store.
<4> CHR prints the contents of the store at the top level by default. The store contains salt_water.

When we call a CHR constraint from prolog, that argument unifies with the same position in the CHR constraint.

[WARNING]
.Don't Bind Variables!
=====================================================================
`foo(1)` is a **different constraint** than `foo(_G123)`.

It **does not work** to ground variables on the left side of the operator.

If you need to ground a variable, do it on the right side.
This is probably cryptic, and definitely a flat assertion.
We will explore it later in the section on head syntax, and when we cover retreiving
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

[WARNING]
.Unbound Variables **still** don't do what you expect!
=====================================================================
`foo(1)` is a **different constraint** than `foo(_G123)`.

foo(N), bar(N) ==> format('foo and bar for ~w present~n', [N]).

?- foo(3), bar(3).    % this works fine

It **does not work** to ground variables on the left side of the operator.

?- foo(1), foo(3), bar(X).
The X is **not** going to be bound to 1 and 3 on backtracking. We're just adding an unbound
bar to the store.

If you need to ground a variable, do it on the right side.

foo(N), bar(M) ==> N = M.

More about this when we cover returning elements from the store.
=====================================================================

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

---------------------------------------------
guard(Intensity) <=> Intensity < 3.0 | true.   > remove at some point, stupid colorizer

---------------------------------------------

Now, a warning. You may NOT bind variables in the guard. A particularly easy
way to do this is to 

[WARN]
.Exercise - Don't Bind Variables in Guard
=====================================================================
The behavior of binding variables in the guard is undefined, and the
CHR compiler will flag it.
=====================================================================

You can have multiple goals in the guard. They execute left to right in the usual Prolog manner.
There is no backtracking (you can't bind anything anyway). They can share variables with the head,
(as long as they don't bind them) and with each other.

Try to minimize use of guards for performance. Every time the rule might fire, the guard must be executed.

Guards have one other very useful property - **Reactivation**. We might have a guard whose success can change
**without adding a constraint to the store**

----
foo(N) <=>  ground(N) | do_something.
bar(N) <=>  N > 3 | do_something.
----

When CHR encounters such a guard, if the guard fails, but might later succeed, as in the first guard,
or the guard is ambiguous as in the second (if N is unbound), the rule does not fire, but a **unification hook** is attached to the variable. When the variable later grounds, the constraint store looks for rules that can fire.

[Exercise]
.Exercise - Intervals
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

CHR execution
-------------

When a constraint is added to the store, CHR adds the constraint, then looks downward from the top of the store until it finds a rule that can fire.  It fires the rule. It  then returns to the top and tries to find a rule to fire again.
When there are no more rules that can fire, the loop returns to the caller.

If the rule adds new constraints on the RHS, then the entire algorithm applies recursively at that point.
So

----
foo ==> bar,baz.

----

We add foo. We find the above rule and add **bar only**. We added a constraint, so we **look for a rule match**.
Now we add baz. We added a constraint, so we **look for a rule match**.


[Exercise]
.Exercise - Does this terminate?
=====================================================================
:- chr_constraint moo/0,bar/0.

moo ==> bar.

?- moo.

Think about this. If I add moo then I make bar, then look for another
rule, find the same one, and keep going. No?
=====================================================================

No. 

rules **do not re-fire** if both of:

The exact same constraints are involved. Thus the salt,salt,water,water example works.
And this answers our 'keep going' question.

Prolog in the body didn't leave choice points.

[Exercise]
.Exercise - How many solutions do I get?
=====================================================================
:- chr_constraint backtrack/1.

backtrack(X) ==> member(X, [a,b,c]).

?- backtrack(X).
=====================================================================

What happens if the Prolog **fails**?

**We're not in Prolog** - so there will NOT be backtracking to a previous solution. Instead, the **entire
attempt** to add the constraint to the store will fail, and the CHR store will roll back to it's previous
state. The original Prolog that added the constraint will fail.

[Exercise]
.Exercise - Demonstrate failure
=====================================================================
Fiddle about and demonstrate failure.
=====================================================================

[Exercise]
.Exercise - 8888
=====================================================================
Let's get lucky. Many Chinese folks think numbers like 8888 are auspicious.

make a constraint lucky/1. Only add to the store those that are 'lucky' (a series
of 8's).
lucky(888).  % stays in store
lucky(77). % remove
=====================================================================


Making CHR interact with Prolog
-------------------------------

As we've seen, calling a CHR constraint from Prolog causes it to be added to the store.

If some prolog fails in rules called by the constraint, the Prolog fails.

If the constraint rules call Prolog and generates choice points, the constraint succeeds with choice points.

If the constraint encounters a guard that might change when 

<<TODO>>  threads

<<TODO>>  server

Getting
-------

What hasn't been mentioned to now is how to get a value back to prolog.

CHR doesn't backtrack on it's own, we can't bind in the head, so how do we get a value back?

If we only expect one of the constraints in the store, this is relatively straightforward.
Make a get_thing constraint that grabs the thing on the RHS.

----
:- chr_constraint thing/1, get_thing/1.

thing(N) \ get_thing(M) <=> N = M.
----

Notice that I'm binding N to M **on the right hand side**.  
This makes the constraints in the head thing with any argument, and get_thing with any argument.
They need not be the same at this point.
On the RHS we're going to hope get_thing's argument is unground, and unify it with thing's argument.

If there are many things, and we don't mind backtracking to get them, this works as well.
EG if you just want to print them out, a repeat-fail list works

----
print_things :-
    repeat,
    get_thing(N),
    writeln(N),
    fail.
print_things.
----

But what if there are many things, and we want to consolidate them in a list?

We use a pattern which I've named the _get_foo_ pattern.

----
:- use_module(library(chr)).

:- chr_constraint foo/1,one_foo/1, collect_foo/1, get_foo/1.   <1>

load_it :-                                                     <2>
    foo(1),
    foo(3),
    foo(7).

% copy constraints to be collected
foo(X), get_foo(_) ==> one_foo(X).                            <3>
get_foo(L) <=> collect_foo(L).                                <4>

% collect and remove copied constraints
one_foo(X), collect_foo(L) <=>                                <5>
          L=[X|L1], collect_foo(L1).
collect_foo(L) <=> L=[].                                      <6>

go(X) :- load_it, get_foo(X).                                 <7>
----

<1> There are four moving pieces. 
* `foo/1`, the constraint. 
* `one_foo/1`, a temporary copy of the constraint
* `collect_foo/1`, a place to hold the list built up so far
* `get_foo/1` the API to the outside Prolog
<2> Let's load some constraints
<3> We'll copy all the constraints, then delete the copies as we collect them into the list
<4> When we start we swap `get_foo` for `collect_foo`, passing along the argument.
<5> we discard a `one_foo`, and the current `collect_foo`. We bind `collect_foo`'s argument **L**
to a **hole list**, a list whose final element is the unbound L1. We then add a new `collect_foo` with
L1 to fill in the rest of the list.
<6> Eventually we run out of `one_foo`'s. We transform the **hole list** to a normal list by 'plugging the hole' with an empty list.
<7> convenience predicate to run the system.

<<TODO>> helpful utilities

Helpful Utilities
-----------------


----
% print out the constraint store
ps :-
    find_chr_constraint(Y),
    format('constraint store contains ~w~n', [Y]),
    fail.
ps.

% print out constraint store when you return to top level
ss :- set_prolog_flag(chr_toplevel_show_store, true).

% or don't
noss :- set_prolog_flag(chr_toplevel_show_store, false).
----

Examples
--------

There is a dearth of good worked examples and exercises for CHR. Most examples are quite short, and give a poor
feel for actually programming in CHR.

Additionally, like any other language, CHR has some standard patterns. 

So I'm going to provide a bunch of simple examples.


Tech Tree
~~~~~~~~~

/* This example solves questions about
 *  "tech trees" using Constraint Handling Rules
 *
 *  Tech Trees are a common game mechanic where the player,
 *  or AI opponent, must build a specific building or unit
 *  before they can build another.
 *  Seeing a unit, the opposing player can then infer that
 *  their opponent possesses all the units needed to
 *  So, for example, if building tanks requires the tank factory,
 *  and the tank factory requires the foundry, then if we see a tank
 *  we can infer they have the foundry.
 *
 *  Our logic doesn't take into account later destruction of units.
 *
 *  Usage is slightly complicated by the fact that returning to the
 *  top level clears the constraint store. Hence provision of the i_saw
 *  convenience predicate.
 *
 *  Usage:
 *  ?- init, i_saw(boat).
 *  ?- init, i_saw([boat, knight]).
 *
 *  stepping in the chr_debugger is useful
 *
 *  ?- chr_trace,init,i_saw(boat).
 *
 *  useful references
 *  https://dtai.cs.kuleuven.be/CHR/files/tutorial_iclp2008.pdf
 *  https://dtai.cs.kuleuven.be/CHR/
 *  https://dtai.cs.kuleuven.be/CHR/webchr.shtml
 *  https://www.swi-prolog.org/pldoc/man?section=chr
 *
 *  Copyright (c) 2019   Anne Ogborn
 *  released under the accompanying MIT license
 */


:- use_module(library(chr)).

% CHR constraints must be defined using this directive
:- chr_constraint
    saw/1, % I saw a unit of this type
    has/1, % I can infer enemy has a unit of this type
    can_build/1, % I can infer enemy can build a unit of this type
    can_build_if/2, % enemy can build a unit of this type if arg2 list all exist
    needs/2,     % game rule, to build arg1, all of list arg2 must exist
    reset/0.     % reset the game world

% reset the game elements
reset \ saw(_) <=> true.
reset \ has(_) <=> true.
reset \ can_build(_) <=> true.
reset \ needs(_, _) <=>true.
reset \ can_build_if(_, _) <=> true.
reset <=> true.

%
% set the initial state of the constraint store,
% with the game dependencies and two initial peasants
%
% to make barracks must have peasant, etc.
init :-
    needs(barracks, [peasant]),
    needs(stable, [peasant, barracks]),
    needs(dock, [peasant, barracks]),
    needs(swordsman, [barracks]),
    needs(knight, [stable]),
    needs(boat, [dock, peasant]),
    saw(peasant),  % we 'saw' the peasant
    saw(peasant).  % because game starts with 2 peasants


% enforce set semantics for various things
% once we know they have a boat, we don't want to add
% that again
saw(X), saw(X) <=> saw(X).
has(X), has(X) <=> has(X).
can_build(X), can_build(X) <=> can_build(X).
can_build_if(X, Y), can_build_if(X, Y) <=> can_build_if(X, Y).

% common sense
saw(X) ==> has(X).         % I saw it, they have it
has(X) ==> can_build(X).   % they have it, they can make it

% this only exists briefly
:- chr_constraint must_have/1.

% expresses the idea 'they have tanks, they must have a tank factory'
% because needs has a list, we recursively fire the must_have rule
% to add everything
has(X), needs(X, List) ==> must_have(List).
must_have([]) <=> true.
must_have([X|Y]) <=> has(X), must_have(Y).

% add can_build for everything whose needs exist
% having dock and peasant adds can_build(boat).
% we wait until the first element of list exists,
% then go on to second element and wait, and so on
needs(X, Z) ==> can_build_if(X, Z).
can_build_if(X, []) <=> can_build(X).
has(Y), can_build_if(X, [Y | Z]) <=> can_build_if(X, Z), has(Y).

% convenience prolog predicate for testing.
% pass list of units seen
i_saw(X) :-
    atomic(X),
    call(saw(X)),
    print_store.
i_saw(X) :-
    is_list(X),
    maplist(callsaw , X),
    print_store.

callsaw(X) :- call(saw(X)).

%
% print out results of computation, showing
% what the enemy has built, and what they can build
%
print_store :-
    writeln('==============='),
    find_chr_constraint(has(Y)),
    format('Your enemy has built ~w~n', [Y]),
    fail.
print_store :-
    nl,
    find_chr_constraint(can_build(Y)),
    format('Your enemy can build ~w~n', [Y]),
    fail.
print_store.
----

Pattern - Adjacency
~~~~~~~~~~~~~~~~~~~

when data comes in as an ordered list we can store it in a constraint like

----
data(Prev, Index, Value)
----

and find adjacent values with a rule like

----
data(N, _, V2), data(_, N, V1) ==> ... .
----

[Exercise]
.Exercise - Sawtooth waves
=====================================================================
Data is provided in a list like this:
[0,1,2,3,5,6,6,7,8,1,2,3,4,6,7,8,3,...]
the data is sawtooths - the value rises gradually, then drops suddenly

End with a set of constraints drop(N) in the store, recording the points where
the data drops.

Stretch goal - assume there's a small amount of noise in the data. Only record drops larger than 
a threshold passed in.

Ultra stretch goal - sometimes the DAC will catch a value during the fall
[1,2,3,4,5,6,7,**5**,1,2,3,4,4,5]

fix your program so it catches these as well.
=====================================================================



* cellular automaton from schrijvers slides  slide 82
* recipe reasoner using drinks
* ref ld45
* radioactive decay
% given half lives, put in atoms and watch'em decay
% keeping track of time

% adventure game.

% 'properly dressed' puzzle

Schrijvers 83 - example of 'generate and filter' pattern (56 for generate pattern)

constraint system example.

Useful patterns
* get_foo
* constraint system
* adjacency
* collection
* backtracking for labeling

More examples

Reactivation - see Schrijvers slide 96
* domain constraint from slides 96

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

summative assessment (add more assement in middle)

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


Conclusion
-----------

<<TODO>>Old clpfd version, fix it

Remember, constraint programming is something you should consider any time you have several variables and need to solve for a compatible solution among all of them.

SWI-Prolog ships with a number of constraint libraries. If you're truly inspired, make a tutorial similar to this one for clp(b) or clp(qr) or CHR. If you're inspired to do this, drop me a line, as we're trying to get a nice library of these tutorials together, with a common format so we can automate parts of the process downstream.

I hope you've enjoyed this tutorial. If you notice any mistakes, or want to suggest improvements, or just are totally stumped, email annie66us (at) yahoo_(dotcom) and let me know.

You can often find me as Anniepoo on ##prolog channel on freenode.net IRC.

Thanks to Markus Triska for the clp(fd) library. Hoot Markus!  Thanks to Alan Baljeu for helping with some confusing points. Thanks to Michael Richter for some of the software pipeline setup that produces these tutorials.
