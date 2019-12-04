Tutorial - CHR Constraint Handling Rules - Basics
=================================================
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

Contents
--------

.The constraint store
.Arguments
.Defining CHR constraints
.Basic CHR syntax
.Making CHR interact with Prolog
.Threads
** Getting
** Helpful Utilities

Basics
------

Here's our _salt water_ example again:

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


The Constraint Store
--------------------

As we have seen, the constraint store holds state. We can put things, called _constraints_, in, and take them out. 

The constraint store can hold multiple copies of a constraint. It works like a multiset or bag.

So, if you make salt water twice, you have two salt_waters. 

[NOTE]
.Exercise - Multiset Semantics
=====================================================================
Use the above program.

What do you think will happen if you query this? Try it

Query 

----
?- salt, water, salt.
----

How about this?

Query

----
?- salt, salt, water, water.
----

=====================================================================
Arguments
~~~~~~~~~

CHR constraints can have arguments of any Prolog type. 
Suppose we have a large number of beakers, and want to make
salt water in them. We can add an argument to record which beaker we
put the salt or water in.

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

When we call a CHR constraint from prolog, that argument unifies with the same position in the CHR constraint. However, the matching is not done in Prolog- it's done in CHR. And to CHR `foo(X)`, with X unbound, is a **different constraint** than `foo(2)`.

[NOTE]
.Exercise - arguments
=====================================================================
Use the multi-beaker version of the program
What do you think will happen if you query this? Try it

Query 

----
?- salt(1),water(2), water(1).
----

How about this?

Query

----
?- salt(1),water(2), water(_).
----

And this?
Query

----
?- salt(1),water(2), water(X),X=3.
----

=====================================================================
Defining chr_constraints
~~~~~~~~~~~~~~~~~~~~~~~~

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

If we leave the salt water for `time`, it will evaporate back to `salt`.
Add a new CHR constraint `time/0` and when you have `salt_water` and `time`
return to `salt`. When you have `water` and time, go back to nothing (put true
on the right).
=====================================================================

Basic CHR syntax
----------------

It's time to introduce the CHR syntax in more detail.
There are a few bits
of syntax which will be omitted here and covered later.

We have seen one operator, `<=>`, informally.  There are actually 3 operators. 

----
name @ discarded <=> guard | body.              <1> Simplification
name @ retained \ discarded <=> guard | body.    <2> Simpagation
name @ retained ==> guard | body.           <3> Propagation
----

All rules have the same structure regardless of their operator.

Name
~~~~

First comes an **optional** _name_, a unique identifier for the rule. This is used by various CHR tools,
but has no effect on the _operation_ of CHR.  _name_ is followed by an `@`  separator if included.
We will not discuss names further.

Head
~~~~

Next comes the _head_.  The head is a series of CHR constraints which must **all** be present for the rule to fire.
In our first salt water example, both salt and water must be present.

As we've already seen, we can store more than one `salt` in the store. We can have two `salt`s in the head, in which case we two or more salts in the store to fire the rule.

[NOTE]
.Exercise - set semantics
=====================================================================
Let's see if we can change the first program, so `salt` means any non-zero amount of salt, 
and `water` means any non-zero
amount of water, and we only get one `salt_water`.

Hint: you want two salts to become one salt.
=====================================================================

This **set semantics** is a frequently useful pattern.


Matching
^^^^^^^^

So what does it mean to match?

Generally this is straightforward - it unifies like Prolog. 

We saw an example of using this earlier, in the multiple beaker
version of salt water.

----
salt(N),water(N) <=> salt_water(N).
----

If there's a `salt(3)` and a `water(4)` then we don't get salt water. The arguments must match.

Now we come to a gotcha.
If you add a constraint from Prolog, be cognizant of **what constraint you are actually adding**.

Suppose you have a constraint `foo/1`, and want to get the values.

----
% doesn't work
get_foo_back(X) :-
    foo(X).
----

This doesn't work because it instead **adds foo with an unbound variable to the store!**

So let's try adding a CHR rule

----
% still doesn't work
get_foo_back(X) :-
    get_foo(X).

foo(A), get_foo(A) ==> true.
----

Assume we have `foo(3)` in the store.
This still doesn't work because CHR does:

. Prolog adds `get_foo(X)` with X unbound
. CHR looks for a rule to fire. It finds `foo(3)` in the store and grounds A to 3.
. CHR then looks for `get_foo(3)` (since we've bound the variable), and, while there's a `get_foo` with an **unbound variable**, there isn't one with a 3. Oops.

Getting information back from the store to Prolog is covered in [Getting](http://#Getting).

Operator
~~~~~~~~

There are 3 operators in CHR.

We have seen the first type of rule, **Simplification**, denoted by the operator `<=>`. 
What is on the left hand side is **discarded** from the store, and then the right hand side(RHS) is done.

Our 'single stir' example has a defect.  It removes the stir from the store, and then re-adds it. This is inefficient and cumbersome. Not only does it mean there are more database operations, but every time a constraint is added to the store, possibly more rules fire. If we have a rule that stirring makes `noise`, we get a new `noise` each time the stir constraint is added (back) into the store.

The **Simpagation** rule type eliminates the remove/re-add workaround. It uses the same operator `<=>` as the **Simplification** operator, but has a
`\` backslash in the head. The constraints to the left of the `\` are left in the store, and those to the right removed.

Let's improve our single stir version.

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

We haven't used a guard yet.  A _guard_ is an optional prolog query. If it succeeds, and if the appropriate
constraints are in the store to match the left hand side, then, and only then, will the rule fire.

Suppose we have a bunch of constraints like `quake(Intensity)`. Our instrument is quite sensitive, and we
have many small tremors we can discard. Let's discard all quakes less than 3.0 .

---------------------------------------------
guard(Intensity) <=> Intensity < 3.0 | true.

---------------------------------------------

You can have multiple goals in the guard. They execute left to right in the usual Prolog manner.
There is no backtracking (you can't bind anything anyway). They can share variables with the head,
(as long as they don't bind them) and with each other.

Try to minimize use of guards for **performance**. Every time the rule might fire, the guard must be executed.

Now, a warning. **You may NOT bind variables in the guard**.
Why this restriction?  The guard will be called whenever Prolog decides to check if
the guard might succeed. 
If the guard binds variables, simply checking a variable can ground it, or another variable.
 
[WARN]
.Don't Bind Variables in Guard
=====================================================================
The behavior of binding variables in the guard is undefined, and the
CHR compiler will flag it.
=====================================================================


[NOTE]
.Constraints
=================================================================
This material requires understanding constraint programming.
If you don't, you can skip over it and know you just shouldn't 
bind variables in the guard.
Alternatively, check out 
link:http://www.pathwayslms.com/swipltuts/clpfd/clpfd.html[my clp(fd) tutorial]
=================================================================

Guards have one other very useful property - **Reactivation**. We might have a guard whose success can change
**without adding a constraint to the store**

----
more_than_3(N) <=>  ground(N) | N > 3.

?-more_than_3(X),writeln('middle'),X = 2.
middle
false.
----

This guard can change if we add `more_than_3(X)` and then later **ground X**.

When CHR encounters such a guard, the rule does not fire, but a **unification hook** is attached to the variable. When the variable later grounds, the rule fires. This may cause **prolog** to fail at this point.

This allows us to build **constraint systems**. 

Note that the above code says 'middle', and only fails when X is bound.

Since we can later change the constraint we can develop efficient constraint checkers.
If we apply `more_than_4` to X then we _subsume_ `more_than_3`. We can discard the weaker constraint.

----
more_than_4(X) \ more_than_3(X) <=> true.
----

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
If a Prolog goal or chr_constraint fails **the entire attempt to add the original constraint fails**, and the store is returned to it's original state prior to the attempt.

You **ARE** allowed to bind arguments in the body. This becomes important when we look at getting data back from the store.

Which Rule Fires?
-----------------

When a constraint is added to the store, CHR adds the constraint, then **looks downward from the top of the store until it finds a rule that can fire**.  It fires the rule. It then returns to the top and tries to find a rule to fire again. There is no backtracking.
When there are no more rules that can fire, the loop returns to the caller.

If the rule adds new constraints on the RHS, then the entire algorithm applies recursively at that point.
So

----
foo ==> bar,baz.

----

We add foo. We find the above rule and add **bar only**. We added a constraint, so we **look for a rule match**.
Now we add baz. We added a constraint, so we **look for a rule match**.

When CHR is 'just sitting there' no constraint is active. When we call a chr_constraint from Prolog, it
is added and made the _active_ constraint. If the rule causes other rules to be added, in turn they will
be the _active_ constraint. Only rules that contain the active constraint are checked.

This makes the store more _stable_. You needn't worry about some unrelated action firing a rule.


[Exercise]
.Exercise - Does this terminate?
=====================================================================

----
:- chr_constraint moo/0,bar/0.

moo ==> bar.

?- moo.
----

Think about this. If I add moo then I make bar, then look for another
rule, find the same one, and _keep going_? No? (think about it, then
keep reading).
=====================================================================

It terminates. 

rules **do not re-fire** if both of:

* The exact same constraints (not just type) are involved. Thus the `salt,salt,water,water` example works.
And this answers our 'keep going' question.
* Prolog in the body didn't leave choice points.

[Exercise]
.Exercise - How many solutions do I get?
=====================================================================
:- chr_constraint backtrack/1.

backtrack(X) ==> member(X, [a,b,c]).

?- backtrack(X).
=====================================================================

What happens if the Prolog **fails**?

If that fails, the **entire attempt** to add the constraint to the store will fail, and the CHR store will roll back to it's previous state. The original Prolog that added the constraint will **fail**.

Notice that what it will NOT do is try other valures or go on to the next rule. Failure stops the entire show.

[Exercise]
.Exercise - Demonstrate failure
=====================================================================
Fiddle about and demonstrate failure.
what happens if you add a constraint to the store and then have the original calling prolog fail?

For that matter, what happens if you throw?

Extra credit: what happens if you throw a delimited continuation and
it resets?
=====================================================================

What happens if the Prolog in the body leaves **choice points** that aren't consumed later in the body?
Then if the **original calling prolog** 'sees' a choice point.

Recursion is often a useful pattern.

[Exercise]
.Exercise - 8888
=====================================================================
Let's get lucky. Many Chinese folks think numbers like 8888 are auspicious.

Make a constraint `lucky/1`. Only add to the store those that are 'lucky' (a series
of 8's).

----
lucky(888).  % stays in store
lucky(888888888). % stays in store
lucky(77). % remove
lucky(8). % stays in store
----

Hint: you will need **two** constraints, one to stay in the store and one to add
to the store
=====================================================================

A bit of larger advice. When recognizing, there is an inherent conflict between adding
things to the store and replacing them.
Suppose we have a recognizer for drawings. It's handed this drawing:

image:house.png[childs drawing of house]

We define a house as a triangle atop a box. Well, if we find the 'triangle' part first,
and turn it into a triangle, we're left with

image:housebody.png["3 lines, 2 vertical and one at bottom"]

Which isn't a 'box' any more. In this case it's probably better to leave 'lines' in the store.

On the other hand, if you recognize a squiggle as a 'line' (as opposed to an S shape or a circle), it's
probably better to remove the squiggle and keep the line.

Making CHR interact with Prolog
-------------------------------

As we've seen, calling a CHR constraint from Prolog causes it to be added to the store.

If Prolog in the **body** of any rule fails, all changes to the store since the 'original'
attempt to add a constraint (by calling it from Prolog) are **rolled back**.
The Prolog itself then fails t that poing.

----

:- chr_constraint blah/0.

some_prolog :-
    writeln('got here'),
    blah,
    writeln('Prolog will fail before it gets here').

blah ==> fail.

?- some_prolog.
got here
false.
----

If the constraint rules call Prolog and generates choice points, the constraint succeeds with choice points.

If the constraint encounters a guard that might change when a variable is grounded, the constraint is subject to **reactivation**. See the section above on [[Guards]]

Threads
~~~~~~~

A CHR store is **local to one thread**. 

This is particularly painful when implementing a server that uses CHR.

One solution is to do all the CHR work on a special thread. 

Falco Nogatz's 
link:https://github.com/fnogatz/CHR-Constraint-Server[CHR-Constraint-Server] is a useful tool.

The 
link:https://github.com/SWI-PrologTeamLudumDare32/LudumDare45['3 Little Pigs' game]
is a useful starter for a server that uses CHR for it's logic.

A Pengine will have it's own thread. This can be useful for CHR.

Getting
-------

What hasn't been mentioned to now is how to get a value back to prolog.

If we only need a generator, to get the solutions on backtracking, this is relatively straightforward.
Make a get_thing constraint that grabs the thing on the RHS.

----
:- chr_constraint thing/1, get_thing/1.

thing(N) \ get_thing(M) <=> N = M.
----

Notice that I'm binding N to M **on the right hand side**.

This makes the constraints matched in the head `thing` with **any argument**, and `get_thing` with **any argument**.
They need not be the same at this point.
On the RHS we're going to hope `get_thing`'s argument is unground, and unify it with `thing`'s argument.

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
Unfortunately `findall/3`, `setof/3`, bagof/3` etc. don't work.

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
to a **hole list**, a list whose final element is the unbound L1. We then add a new `collect_foo` with L1 to fill in the rest of the list.
<6> Eventually we run out of `one_foo`'s. We transform the **hole list** to a normal list by 'plugging the hole' with an empty list.
<7> convenience predicate to run the system.

Helpful Utilities
-----------------

There is a debugger for CHR similar to the old four port debugger. `chr_trace/0` and
`chr_notrace/0` invoke it.

----
% put this right at beginning for production
% disable tracing (makes it run faster by  removing debug )
:- chr_option(debug, off).
% let it optimize
:- chr_option(optimize, experimental).

%slower execution, easier understanding of debugger output
 :- chr_option(debug, on).  :- chr_option(optimize, off).
----

CHR clears the constraint store when it returns to the top level.
This can be annoying when learning or debugging if you want to manually
manipulate the store.

Here are a couple ways to avoid this. The first repeatedly breaks into
a new level of the interactor. The second is to use a flag that does effectively
the same thing, but you don't have to remember to do it each time.
 
----
 % avoid the constraint store evaporating at top level.
 ?- run_my_program, break.

% saner way to do same
?- set_prolog_flag(toplevel_mode, recursive).
----

`current_chr_constraint/1` is intended for debugging, and is slow.
Don't use this as a pattern for reading the constraint store.
but I often add this to my CHR programs as a handy debug tool.

----
% print out the constraint store
ps :-
    current_chr_constraint(M:Y),
    format('constraint store contains ~w:~w~n', [Y]),
    fail.
ps.
----

CHR prints out the contents of the constraint store when you return to the top
level.  This behavior can be helpful or annoying dependin on the situation.

Here's some debug code to turn it on and off.

----
% print out constraint store when you return to top level
ss :- set_prolog_flag(chr_toplevel_show_store, true).

% or don't
noss :- set_prolog_flag(chr_toplevel_show_store, false).
----

Conclusion
----------

This completes the basics of CHR. You should be ready to study 
link:constraintsystems.html[Constraint Systems]

This is also a great time to work some of the 
link:examples.html[Examples and Patterns]

Useful references

 *  https://dtai.cs.kuleuven.be/CHR/files/tutorial_iclp2008.pdf
 *  https://dtai.cs.kuleuven.be/CHR/
 *  https://dtai.cs.kuleuven.be/CHR/webchr.shtml
 *  https://www.swi-prolog.org/pldoc/man?section=chr

