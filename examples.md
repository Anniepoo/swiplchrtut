Tutorial - CHR Constraint Handling Rules - Examples and Patterns
================================================================
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

This chapter covers no truly new material, but should help you find some useful ways to use CHR.


.Useful patterns
** get_foo
** constraint system
** adjacency
** set semantics
** uniqueness
** generate and filter
** actions and state
** collection/aggregation
** speeding up graph traversal
** backtracking for labeling
. Implementing Things in CHR


Useful Patterns
---------------

get_foo
~~~~~~~

This pattern is used to get a list of constraints from the store.

It was covered in 'getting', but is largely repeated here.

The trick is to pass in an unbound variable with `get_foo`, and
then bind the variable in rule bodies.

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

Constraint System
~~~~~~~~~~~~~~~~~

See the chapter on constraint systems.

Adjacency
~~~~~~~~~

We may have an ordered list of items, and want to put them in a constraint store while preserving their
order information. In particular, we might want to preserve adjacency.

Here are a few strategies

Keep an index
^^^^^^^^^^^^^

Store constraints as `foo(Index, Data)`. Check adjacency as

----
foo(I, Data1), foo(J, Data2) ==> succ(I, J) | something(Data1, Data2).
----

Keep Index Pairs
^^^^^^^^^^^^^^^^

Store constraints as `foo(Index, Next, Data)` where Next is Index + 1.

----
foo(_, I, Data1), foo(I, _, Data2) ==> something(Data1, Data2).
----

This takes more memory but runs faster.

This strategy also allows a form of **bottom up parsing** where you replace raw tokens with
progressively more 'cooked' tokens. Suppose we have some tokens already classified as `word_char(Char)`,
we can assemble words as:

----
token(WordIndex, Letter, word(Word)), token(Letter, Next, word_char(B)) <=> 
       append(Word, [B], NewWord),
       token(WordIndex, Next, word(NewWord)).
token(WordIndex, Next, word_char(A))  <=> token(WordIndex, Next, word([A]))
----

Set Semantics
~~~~~~~~~~~~~

Sometimes it's useful to give a CHR constraint set semantics instead of the default multiset semantics.

Model of light in a room

----
:- chr_constraint turn_on/0, turn_off/0, light_on/0.

turn_on <=> light_on.
turn_off, light_on <=> true.
----

The problem is, if the light is already on when turn_on occurs, we get two `light_on` constraints
in the store. Not what we want - `turn_off` won't work properly.

Add a rule

----
light_on \ light_on <=> true.
----

Uniqueness
~~~~~~~~~~

Want to enforce uniqueness?

Heather has two mommies, but only one biological mother.

----
bio_mom_of(X,Y) \ bio_mom_of(Z, Y) <=> X = Z.
----

First, we are demanding that if there are two `bio_mom` constraints for a child,
then they must be for the same person. That's a long winded way of saying "only one bio_mom".

Second, we are enforcing set semantics - if they are the same, we discard one.

Generate and Filter
~~~~~~~~~~~~~~~~~~~

It is often easier to generate a set of something and then filter out items not in the set.

Here's a (rather inefficient) prime number generator:

----
:- chr_constraint init/1,num/1.
% primes

% generate numbers from 1-n
init(N) ==> num(1).
init(N), num(M) ==> N > M | succ(M, NewM), num(NewM).
num(N) \ init(N) <=> true.

% remove the non-primes.
num(A) \ num(B) <=> B > A, B mod A =:= 0 | true.
----

Actions and State
~~~~~~~~~~~~~~~~~

I find it useful to distinguish _actions_, constraints in the store to set off behavior, from _state_.

In the above prime number example, we know we should make num/1 constaints when init/1 exists.

The general pattern is:


. Outside agency (prolog or other CHR) adds the 'ignition' constraint
. Rules fire until some process completes
. The 'ignition' constraint is removed.

Collection/Aggregation
~~~~~~~~~~~~~~~~~~~~~~

If you want to **maintain** an aggregate total, you can detect adding and removing
the constraint:

----
% add and remove foo's with numeric argument, keeping running total
:- chr_constraint foo/1, total_foo/1, unfoo/1.

total_foo(A), total_foo(Total) <=> NewTotal is A + Total, total_foo(NewTotal). 
foo(A) ==> total_foo(A).

unfoo(A), foo(A), total_foo(Total) <=> NewTotal is Total - A, total_foo(NewTotal).
----

You can do aggregation / foldl / collection in the same way get_foo works.

----
:- use_module(library(chr)).

:- chr_constraint foo/1,one_foo/1, sum_foo/1, do_sum_foo/0.   <1>

load_it :-                                                     <2>
    foo(1),
    foo(3),
    foo(7).

% copy constraints to be collected
foo(X), do_sum_foo ==> one_foo(X).                            <3>
do_sum_foo <=> sum_foo(0).                                    <4>

% collect and remove copied constraints
one_foo(X), sum_foo(N) <=>                                    <5>
          NN is N + X, 
          sum_foo(NN).

go(X) :- load_it, do_sum_foo, find_chr_constraint(sum_foo(X)).   <6>
----

<1> There are four moving pieces. 
* `foo/1`, the constraint. 
* `one_foo/1`, a temporary copy of the constraint
* `sum_foo/1`, a place to hold the list built up so far
* `do_sum_foo/0` the action constraint that sets it off
<2> Let's load some constraints
<3> We'll copy all the constraints, then delete the copies as we sum them
<4> Next we create `sum_foo`.
<5> we discard a `one_foo`, and the current `sum_foo`. We add a new `sum_foo` with the new sum. Eventually we run out of `one_foo`'s and sum_foo contains the total.
<6> convenience predicate to run the system. 


Speeding Up Graph Traversal
~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you have a small graph you traverse many times, it may be worth adding edges
to directly go to indirectly reachable nodes.

----
% make all connected edges
% if you have a-b b-c c-d
% add a-c b-d a-d 

:- chr_constraint node/2.
node(A,B) \ node(A,B) <=> true.     % anti-cycle

node(A,B), node(B,C) ==> node(A,C).

?- node(1,2), node(2,3), node(3,1).

query(5, 2, X).

:- chr_constraint query(+, +, -). % uses material from advanced chapter
node(A,D), node(B,E)
\ query(A,B,C)
<=> C is E + D.
----

Backtracking for Labeling
~~~~~~~~~~~~~~~~~~~~~~~~~

Constraint systems usually provide a `label` predicate to force grounding of all variables.

CHR's reversal on backtracking provides a nice way of writing a labeling predicate for your
constraint system.

Implementing Things in CHR
--------------------------

Here's the list of applications we started this tutorial with. Now that you have some understanding of CHR,
let's look at how we'd implement most of these. I omitted a few as being obvious or unedifying.

writing constraint systems
~~~~~~~~~~~~~~~~~~~~~~~~~~

When constraint B subsumes constraint A, discard A for better performance
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

----
must_be_more_than(X, Y) \ must_be_more_than(X, Z) <=> Y >= Z | true.
----

When there is a route from power to ground, we have a short
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This assumes the student knows a little about electronic circuitry.

----
:- chr_constraint   at_voltage/2, short/1, wire/2.

% if we are holding a pin at two different voltages there's a short
at_voltage(V1, T), at_voltage(V2, T) ==> V1 \= V2 | short(T).
% connections to a pin from a voltage source hold that pin at the source
% vcc is the power supply voltage, gnd is ground
wire(vcc, Term) ==> at_voltage(vcc, Term).
wire(gnd, Term) ==> at_voltage(gnd, Term).
% if T1 and T2 have a wire between them and one end is held at a voltage,
% the other end is too
wire(T1, T2), at_voltage(V, T1) ==> at_voltage(V, T2).
wire(T1, T2), at_voltage(V, T2) ==> at_voltage(V, T1).
----

Build a constraint system like clp(fd)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Obviously this is beyond the scope of a tutorial, but there are examples in the chapter on
[Constraint Systems](constraintsystems.html)

Build a type system
~~~~~~~~~~~~~~~~~~~

A (dynamic) type is just a constraint to a specific type.

----
must_be_int(X) <=> ground(X) | integer(X).
----

alternatively

----
must_be_int(X, Context) <=> ground(X) | throw(error(type_error(integer, X), Context)).
----

Constraint systems are often far faster than hand rolled algorithms
See [[When constraint B subsumes constraint A, discard A for better performance]]

Making programs where things transform into things
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A GUI where a button changes appearance when clicked
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Here's code for a button. You'd have to add an action triggered by
appearance(B, Appearance) to trigger some prolog that physically changed the button.

You can wire behaviors to buttons by rules triggered by do_action.

----
% came down over B
button(B) \ appearance(B, up), mousedown(B) <=> appearance(B, active).
% mouse is down and user moved off the button
button(B) \ appearance(B, active), mouseleft(B) <=> appearance(B, up), pending_button(B).
% came back on - we stored pending_button above so we only activate the original down button
button(B) \ appearance(B, up), mouseentered(B), pending_button(B) <=> appearance(B, active).
% mouseup with the mouse over the button
% dispatch the action and start an animation. the start_animation prolog predicate adds
% tick to the store every 0.2 seconds for 2 frames
button(B) \ appearance(B, active), mouseup <=> do_action(B), appearance(B, click0), start_animation(2, 0.2).
% mouseup with the mouse not over button
button(B) \ pending_button(B), mouseup <=> true.
% handle the animation
appearance(B, click0), tick <=> appearance(B, click1).
appearance(B, click1), tick <=> appearance(B, up).
----

A game where a a spaceship changes appearance after it is hit, fires it's rockets, etc.
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Here's a bit of CHR code that processes hits on spaceships, and some refactors.

----
% ignore hits on dead ships
ship(S), dead(S) \ hit(S, _) <=> true.
% die if ship falls below 0 health
ship(S) \ health(S, H), hit(S, Damage), appearance(S, _) <=> 
                H - Damage > 0 | 
                appearance(S, damaged), 
                NH is H - Damage, 
                health(S, NH).
ship(S) \ health(S, H), hit(S, Damage) <=> 
                H - Damage =< 0 | 
                dead(S).
ship(S), dead(S) \ appearance(S, _) <=>
                appearance(S, dead).
----

There's a better CHR pattern. The ship is either `ok`, or `damaged`, or `dead`. 
So better to have `health_state(S, damaged)` than `dead(S)`.

Secondly, I decided already to separate the ship's state from it's appearance.
If later we want to have a power up that makes the player's ship look dead, so
enemies ignore it, we need to have the appearance separate from the 'model'. Good
MVC pattern.
But we have the appearance code mixed in with the model code. Let's separate concerns.

A third pattern is that I check S is actually a ship. If only ships have health or health_state, I
can remove that check. Alternatively, I can have `ship_health_state` instead of `health_state`
so we're assured this is just a ship. 

lastly, we can separate the concern of updating the ship's health from it's state.

Let's rewrite it with those changes.

----
% ships start out ok state with 20 hit points
ship(S) ==> health_state(S, ok), health(S, 20).

% ignore hits on dead ships
ship(S), health_state(S, dead) \ hit(S, _) <=> true.

% take damage
ship(S) \ health(S, H), hit(S, Damage) <=> 
                NH is H - Damage, 
                health(S, NH).

% die if ship falls below 0 health
ship(S), health(S, H) <=>
         H =< 0 | 
         health_state(S, dead).

% for now appearance is just state
ship(S), health_state(S, State) ==> appearance(S, State).

----

This version, in my opinion, comes much closer to **expressing our design intent**.


A simulation like a virtual chemistry lab, where adding A and B to the beaker makes C
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This is essentially the salt water example we started with.


An interactive fiction game, where the character moves from room to room
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Here's a simple game where the player can move among 3 rooms. They must pick up
a key to get into the library. For simplicity, you can only go east, west, and pick_up (the key).

----
:- chr_constraint init/0, description/2, player/1, pick_up/0, key/0, look/0.

init ==> description(bedroom, 'an ordinary bedroom. The bed is not made.'),
         description(living_room, 'Bob\'s living room. The picture matches his sofa. there is a key here.'),
         description(library, 'Bob\'s library. Mostly Harlequin romances and Readers Digest.').
init \ player(_) <=> player(bedroom).
iniy \ key <=> true.

% moves
player(bedroom), east <=> player(living_room).
player(living_room), east, key <=> player(library).
player(library), west <=> player(living_room).
player(living_room), west <=> player(bedroom).
player(living_room) \ pick_up <=> key.
east <=> writeln('you can\'t go east here').
west <=> writeln('you can\'t go west here').
pick_up <=> writeln('nothing to pick up here').

% printing

% make the living_room description change when the key's picked up
key \ description(living_room, _) <=> description(living_room, Bob\'s living room. The picture matches his sofa.').

% print the player's location
player(Loc), description(Loc, Desc) ==> writeln(Desc).

% handle look command

look, player(Loc), description(Loc, Desc) ==> writeln(Desc).
look, key ==> writeln('you are carrying a key').
look <=> true.
----

A few subtleties - The **look** command uses the order to ensure we process everything we need - print out
the player's location description, and then tell the player they're carrying the key if they are.
Then we destroy the look command.

bottom up recognizers - input characters to words to clauses to sentences to paragraphs.
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

DCGs are what immediately comes to mind when we think about parsing in Prolog.

But the parsers generated can have issues with left recursion, and with performance. Additionally, they
are most natural for languages like computer languages that are defined recursively.

[quote, David Warren, XSB Book]
____
"It is for this reason that some people respond to the claim that _You get a parser for free with Prolog_ with _Maybe, but it's not a parser I want to use_. "
____

CHR makes it easy to do parsing _bottom up_, building up words, then sentences, and so on.

As a simple example, let's build a CHR program that builds up sentences as lists of words.
For convenience, we'll convert the words to atoms.

As is often useful in Prolog, we'll build the words and sentences up in **reverse**, and flip them with
`reverse\2` at the end.

Some outside Prolog will repeatedly add char/1 letters to the store. The argument is the ASCII value of the
character.

Our example just builds up sentences in the
store and prints them out. We'll also lowercase incoming text and discard punctuation, a common NLP need.

Our example defines a sentence as anything followed by a period.

----
:- chr_constraint char/1, word_done/0, letter/1, partial_word/1, partial_sentence/1, sentence/1, sentence_done/0.

% is it a letter?
char(X) <=> X >= 0'a , X =< 0'z | letter(X).
char(X) <=> X >= 0'A , X =< 0'Z | letter(X).
char(0'.) <=> sentence_done.
char(X) <=> word_done.   % everything else ends words

letter(X), partial_word(List) <=>
            partial_word([X | List]).
letter(X) <=> partial_word([X]).

word_done, partial_word(List), partial_sentence(Sentence) <=>
    partial_word_word(List, Word),
    partial_sentence([Word, Sentence]).
word_done <=> true.  % eg comma followed by space we just ignore the space

sentence_done, partial_sentence(S) <=>
    reverse(S, Sentence),
    sentence(Sentence).

partial_word_word(List, Word) :-
    reverse(List, CodeWord),
    atom_codes(Word, CodeWord).
----

Notice that I get rid of things from the store as soon as they're incorporated in something bigger. 
Letters are includeded in the current partial word and **then discarded**.

Note also that I uses `partial_word_word/2` to convert the list - there seemed little point in doing
this in CHR. The Prolog is both more efficient and easier to write.


A web application where POST requests modify the store, subject to complex constraints.
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Of course a web application is a bit complex for a tutorial. I'll refer you to an example.

[SWi-Prolog](https://swi-prolog.org/) occasionally organizes teams to participate in [Ludum Dare](ldjam.com),
a large international game jam competition.

In summer 2019 the team built a fun little game where you draw childs drawing 'houses' to save a little pig
from a bad wolf. The [source code is here](https://github.com/SWI-PrologTeamLudumDare32/LudumDare45).

The front end of the game is in [Snap!](https://snap.berkeley.edu/), a visual programming environment of the 'drag blocks' type.

an alternative to setting up Snap! locally (which is easy) is to use curl to exercise the system.

A few things to observe.

* Single threaded - all CHR happens in a single thread. We wanted a single constraint store for this application and the constraint store is **per thread**.
* some parsing on the Prolog side. Our client (the **Snap!** game) had crude HTTP support (why we use GET requests). It was easier in practice to do the first bit of parsing in prolog, so we did.

The code starts by decoding the line segments the user has drawn from the input stream of numbers. 

Then it starts identifying 'bigger' pieces.  It tests geometry to identify vertical and horizontal lines.
A vertical and a horizontal line that are close at a corner make a 'corner'.  A pair of 'corner's make a 'box'.
two slanted lines that touch at the end make a triangle, which might have a third, horizontal or vertical side.
A triangle atop a box is a 'house'. The code has the start of recognizing 'rocket'.

Note that the 'house' code is actually quite short. The more things we recognize, the more we CAN recognize.

Note also that it can become tricky - if I replace the 3 lines of the 'roof' with a 'triangle', I lose the top of my 'box'.
And if I recognize a house, well, isn't a vertical rocket a tall thin house with fins? This sort of competing recognition
can become a real software design issue, but a bit of care makes it tractable.

Notice that the application needn't be a game. the system could be maintaining 'session' term state for a business application.

The button example above shows a common need with web applications. State is held on the server, and then we want to send
only the changes to the client.

Suppose we have a web page with various lists of items, say a list of methods of payment. When the browser sends a request to add a method, we respond telling the client side JS to rebuild the payment method list. If they just change an item within one payment method, we tell the client to rebuild only that item. CHR is excellent for building this sort of coordination between change and current state.

I personally think there's a lot of possibility in melding CHR and tau-Prolog to make a fully declarative web application development system.

Writing **Recognizers**
~~~~~~~~~~~~~~~~~~~~~~~

A **Recognizer**, for our purposes, is a CHR program that takes some input and 'finds' a pattern within it.

For an example of doing more general recognition, see the game referenced in [[A web application where POST requests modify the store, subject to complex constraints.]]

A traditional bottom up parser
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Isn't your 'recognizer' just a parser?

Yes, it is, but we associate parsers so strongly with objects that are ordered, like an input string, that I've used
a different word for 'find things in this mass of unordered data'.

For an example of doing traditional parsing in CHR, see [[bottom up recognizers - input characters to words to clauses to sentences to paragraphs.]] above.

A computer vision system that recognizes simple objects like a chimney, a door, etc. and combines them to make 'house'.
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

When I wrote this I was imagining starting with a raster image like a photograph in which we want to find various objects.

For example, say we want to find houses, and on them, doors. 

It's rather hard to build a door recognizer using machine learning -  they are, like many things, just rectangles, and
have a large variety of context (porch, bushes, etc). But it's easier to build a house recognizer. So we build a good house
recognizer and a bad door recognizer, and maybe a passable window recognizer and a good chimney  recognizer.

And we take the output of those, and combine them in CHR, and use symbolic recognition similar to the drawing game, and if
the combination of windows, door, chimney, and 'house' doesn't make sense, we can suspect a false positive.

Writing Expert Systems
~~~~~~~~~~~~~~~~~~~~~~
Most expert systems are just forward chaining inference engines. 

The classic 'room layout' example is an easy exercise in CHR. In this expert system we find solutions
to room arrangements that satisfy some constraints, like the sofa must face the TV, the lamp must be near
a socket and the reading chair, and so on.

Such an expert system uses the **generate and filter** strategy. Code, in CHR or Prolog, generates a room configuration,
and the CHR system confirms that it is a valid arrangment, or **fails**, making us backtrack and try again.
Here's a simple version that enforces a reasonable arrangement for my own living room.

----
:- chr_constraint  place/2.

find_placement :-
    member(Sofa, [n, s, e, w]),
    place(sofa, Sofa),
    member(TV, [n, s, e, w]),
    place(tv, TV),sof
    member(Desk, [n, s, e, w]),
    place(desk, Desk),
    format('place sofa on ~w, tv on ~w, desk on ~w~n', [Sofa, TV, Desk]).

place(sofa, n) ==> fail.  % door to kitchen doesn't leave enough space for sofa
place(sofa, s) ==> fail.  % outside door and puja niche don't leave enough space for sofa
place(sofa, X), place(tv, Y) ==> opposite(X,Y).  % demand sofa and tv are on opposite walls
place(sofa, X), place(tv, Y) ==> fail.
place(desk, n) <=> fail.  % don't place the desk on the north wall, no room.

opposite(n, s).
opposite(s, n).
opposite(e, w).
opposite(w, e).
----

We are depending on the fact that when the body fails the original call to add a CHR constraint fails.
So, for example, we start by trying to add the sofa on the north. The first CHR rules overrides us, 
we can't put the sofa on the north.
It really doesn't matter if we use `==>` or `<=>` here. In the end, there won't be a change to the constraint
store if it fails.

Notice that this is much more efficient than an equivalent **Prolog generate and test** strategy.
placing the TV at a location that's not opposite immediatey is a fail, we don't need to try to place
the desk.

recognizers in expert systems
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Suppose we have a medical expert system. 

[Acute abdomen](https://en.wikipedia.org/wiki/Acute_abdomen) is a serious medical emergency with many possible causes.r
Before we can proceed to a diagnosis, we have to recognize that the patient does indeed have _acute abdomen_.

* patient must have severe abdominal pain
* if the patient had a sudden onset, then it's _acute abdomen_.
* if the patient report of pain onset is vague, these observations improve the diagnosis
** bloody stools
** constipation
** fever

Once we've recognized _acute abdomen_ we can reason about possible causes and figure out what tests to run first.

(obviously, don't use this CHR tutorial for medical advice. See a doctor.)

resource utilization - I have an extra gate on this IC and need one for this.
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

"Linear Logic" is a logic of finite resources. In a conventional logic, there is an ever increasing amount of 
facts known. 

However, when reasoning about the real world, it is convenient to think about a world where making a thing consumes another thing.


----
cake_pan, oven \ cake_mix, eggs, water <=> cake.
----

When we bake a cake, we destroy the mix of ingredients from which it's made, but not the cake pan or oven.

CHR is an excellent method of reasoning about such problems.

A classic linear logic problem is to simulate the action of a soda machine.

----
quarter, quarter, quarter <=> soda.
----

If you put in 3 quarters, you get back a soda.

If done in a **generate and filter** pattern, we can find solutions to complex resource management problems.

"design constraint" systems - like the checker tool before submitting a PCB design
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Suppose we have a design constraint that every pin has at least 2 wires connected, as it might be in an automated
wire-wrap board. We can check a constraint like this

----
design_check, wire(T, _) ==> pin_not_checked(T).
design_check, pin_not_checked(T) \ pin_not_checked(T) <=> true. % get rid of duplicates
design_check, wire(T, A), wire(T, B) \ pin_not_checked(T) <=> A \= B | true.
design_check, pin_not_checked(t) <=> fail. % or do whatever's appropriate when the design fails
----

we need `design_check` because the design won't be valid until we've added all the wires.

Implication
~~~~~~~~~~~

Things imply other things. This is at the heart of the forward chaining nature of CHR.

Tech Tree
^^^^^^^^^

An AI player for a game with a 'tech tree', where players must build the **tank_factory** to build tanks. Seeing a **tank** means they have the **tank_factory**.

This example is available in the examples folder of this tutorial.

Reasoning about mutable state
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A satellite imagery analysis system sees a combine in a field. That implies the crop is being harvested.
During harvest season the local grain elevator will be full. A train will come to empty the elevator.

PCB Traces
^^^^^^^^^^

----
wire(T1, T2), location(T1, X1, Y1), location(T2, X2, Y2) ==>
        outline_trace(X1, Y1, X2, Y2, List), % Prolog to do the geometry, not shown
        polygon(List).
----

In a real example, there would be rules to union the polygon with others.


Search
~~~~~~

Flexibly turning normalized data that has to be accessed via joins into single lookup data.
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Suppose we have some constraints that resemble normalized database tables. 

----
employee(12345, 'Sally Smith', 1234567890).
position(12345, manager, outside_sales).
salary(12345, 69400, 0.1).
----

While this usually is the best way to represent the data, we sometimes want to examine data in
denormalized form.

----
employee(Num, Name, SSN), position(Num, Position, Dept) ==> emp(Num, Name, SSN, Position, Dept).
----

Now searches of the database don't require rules with multiple heads.

Of course this is a memory/speed tradeoff.

Conclusion
----------

Some additional examples are available at the [WebCHR Online CHR Tool](http://chr.informatik.uni-ulm.de/~webchr/)

It's impossible to learn CHR without doing a lot of work in it. Hopefully this chapter has given
you some experience with real CHR programs.

You're now ready to move on to the [Advanced](/advanced.html) material.

If you haven't worked through the [Constraint Systems](/constraintsystems.html) chapter, I suggest doing that before doing the advanced material.




