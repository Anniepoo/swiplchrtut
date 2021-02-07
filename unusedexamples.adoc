
* Dynamic type systems. 
** True type systems
** constraints as a substitute for 'real' type systems.


<<TODO>> - go back to the 'things you can build' from intro and explain how to build each one



[Exercise]
.Exercise - radioactive decay
=====================================================================
We have some isotope `americium_241` which decays to `neptunium_237` and `helium_4`.

we init some americium into the system, and then time in `tick`s passes. At each `tick`
randomly 10% of the remaining americium decays.

Simulate this system using CHR.

?- americium_241,americium_241,americium_241,americium_241,americium_241,tick,tick,tick.
americium_241,
americium_241,
americium_241,
neptunium_237,
helium_4,
neptunium_237,
helium_4.

stretch goal - Pick something whose daughter products are unstable and make
a similar simulator for it, decaying the daughters along with the parent.

=====================================================================


Tech Tree
~~~~~~~~~

This example solves questions about
"tech trees" using Constraint Handling Rules

Tech Trees are a common game mechanic where the player,
or AI opponent, must build a specific building or unit
before they can build another.
Seeing a unit, the opposing player can then infer that
their opponent possesses all the units needed to
So, for example, if building tanks requires the tank factory,
and the tank factory requires the foundry, then if we see a tank
we can infer they have the foundry.

Our logic doesn't take into account later destruction of units.
Even if we blow up the tank factory, can we be sure they don't have others?

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
* parsing - words
* recipe reasoner using drinks
* ref ld45
* radioactive decay
% given half lives, put in atoms and watch'em decay
% keeping track of time
adventure game.
'properly dressed' puzzle
Schrijvers 83 - example of 'generate and filter' pattern (56 for generate pattern)
constraint system example.

Useful patterns
* get_foo
* constraint system
* adjacency
* generate and filter
* collection
* backtracking for labeling
* using order for recognition priority

More examples

Reactivation - see Schrijvers slide 96
* domain constraint from slides 96


