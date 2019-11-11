/*
 *  CHR syntax, since I can never remember it:
 *  name @ retained \ discarded <=> guard | head,body.    Simpagation
 *  name @ discarded <=> guard | head, body.      Simplification
 *  name @ retained ==> guard | head, body.         Propagation
 *
*/
:- use_module(library(chr)).


:- chr_constraint
    foo/1,
    bar/1,
    mep/1,
    zap/1.

foo(X) ==> member(X, [a,b,c]), bar(X).

% doesn't work
% mep(L) ==> member(X, L), zap(X).

mep(L) ==> foreach(member(X, L), zap(X)).

% how do I negate?
:- chr_constraint nord/0, noodge/0, dingle/0.

nord ==> \+ find_chr_constraint(noodge) | dingle.

better
nord, noodge  ==> true.
nort ==> dingle.

% Examples to write
%
% cellular automaton from schrijvers slides  slide 82
%
%domain constraint from slides 96
%
%
% toy project - recipe reasoner
%  (use drinks)

% toy project - radioactive decay
% given half lives, put in atoms and watch'em decay
% keeping track of time

% adventure game.

% 'properly dressed' puzzle

%patterns
% domain constraint from slides 96 - pattern, guard for instantiation
% to implement constraint solver
%

% backtracking undoes changes to constraint store
%

% pattern - backtracking for labeling
%

% pattern - get_gcd is a chr_constraint used only
% to get the value of another constraint.
% Not sure if this backtracks
% it does NOT, apparently
%
% Note that the binding happens in the BODY, cause binding in the
% head is a nono
%
% constraint to get the current gcd/1 value
% gcd(N) \ get_gcd(M) <=> M = N.
%
% that won't backtrack, this will
% gcd(N), get_gcd(M) ==> M = N.
% get_gcd(_) <=> true.
%
% that doesn't backtrack either

% This is the canonical get_ pattern per Thom Fruewirth

% no backtrack get
foo(X) \ get_foo(Y) <=>
         X = Y.
get_foo(_) <=> fail.

% Yay, backtracking get that ACTUALLY WORKS

========== entire file =========
:- use_module(library(chr)).

:- chr_constraint foo/1,one_foo/1, collect_foo/1, get_foo/1.

load_it :-
    foo(1),
    foo(3),
    foo(7).

% copy constraints to be collected
foo(X), get_foo(_) ==> one_foo(X).
get_foo(L) <=> collect_foo(L).

% collect and remove copied constraints
one_foo(X), collect_foo(L) <=>
          L=[X|L1], collect_foo(L1).
collect_foo(L) <=> L=[].

go(X) :- load_it, get_foo(X).

=================


/*
:- use_module(library(chr)).

:- chr_constraint foo/1, get_foo/1.

load_it :-
    foo(1),
    foo(3),
    foo(7).

foo(X), get_foo(Y) ==>
         X = Y.
get_foo(_) <=> true.

%  library(chr) compiled into chr 0.52 sec, 133 clauses
% /home/anniepoo/prologhelp/chrtest compiled 0.53 sec, 57 clauses
?- load_it, find_all(X, get_foo(X), L).
Correct to: "findall(X,get_foo(X),L)"? yes
L = [],
foo(7),
foo(3),
foo(1).

?- 

So, this is ugly as hell, but works

all_foos(L) :-
    nb_setval(all_foos_val, []),
    af(L).

af(_) :-
    find_chr_constraint(foo(X)),
    nb_getval(all_foos_val, OldL),
    nb_setval(all_foos_val, [X | OldL]),
    fail.
af(L) :-
    nb_getval(all_foos_val, L).

doesnt work

foo(X) \ gather_foos_together(Old) <=> gather_foos_together([X | Old]).

gather_foos_together(Foos), get_thy_foos(Y) ==> Foos = Y.
% get_thy_foos(_) <=> true.   don't do this

*/


		 /*******************************
		 *     helpful utilities        *
		 *******************************/

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



%
% name @ retained \ discarded <=> guard | head,body.    Simpagation
% discarded <=> guard | head, body.              Simplification
% retained ==> guard | head, body                Propagation

mumble, mumble, mumble <=> writeln('3 mumbles') | true.
mumble ==> mumble.

doorbell :-
    mumble.



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

% my_con(A,3,B), my_con(B, 3, C) ==> something.

*/

/*
 *
 *

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


:- chr_constraint my_con(+dense_int, +dense_int, +dense_int).

my_con(A,3,B), my_con(B, 3, C) ==> something.

% make all connected edges
a-b b-c c-d
a-d
:- chr_constraint node/2.
node(A,B) \ node(A,B) <=> true.     % anti-cycle

node(A,B), node(B,C) ==> node(A,C).

?- node(1,2), node(2,3), node(3,1).

query(5, 2, X).
:- chr_constraint query(+, +, -).
node(A,D), node(B,E)
\ query(A,B,C)
<=> C is E + D.

% print out execution without pausing
?- chr_leash(-all).  chr_trace.  query(2,3,N).

% better to have init ==> task(customize),task(help),security(csrf_in_forms)...
% even better , since we don't want init
init <=> task(customize),task(help),security(csrf_in_forms)...
% but even better, just do it in prolog
init :- task(customize),task(help),security(csrf_in_forms).
% instead of
init ==> security(remove_test_reset).
init ==> task(implement_localization_hook).  % customize
init ==> task(set_setting(identity:style)).  % customize
init ==> task(attach_database).

?- listing(init).

a \ a <=> true.

% keep all_email_tasks, and only add once using
all_email_tasks \ all_email_tasks <=>

:-chr_constraint  all_email_tasks(+dense_int, +dense_int, +dense_int).

% only do it once pattern
all_email_tasks(    RI,  A,  B)
\ create_all_email(RI ,B)
<=>  true.

create_all_email(RI ,B)
<=>  gen_sym(A), all_email_tasks(    RI,  A,  B).

?- create_all_email(RI ,B), create_all_email(RI ,B).

% this is Ok
completed(decision(remember_me), X) ==> X \= none | all_remember_me_tasks(X).

user(X, UserData)
, completed(decision(remember_me), X) ==> X \= none | all_remember_me_tasks(X).

% put this right at beginning for production
% disable tracing (makes it run faster by  removing debug )
:- chr_option(debug, off).
% let it optimize
:- chr_option(optimize, experimental).

%slow, easy
 :- chr_option(debug, on).  :- chr_option(optimize, off).

 % avoid the constraint store evaporating at top level.
 ?- run_my_program, break.

% saner way to do same
?- set_prolog_flag(toplevel_mode, recursive).


ttmrichter

[21:35] <ttmrichter> I'm simulating a CPU core.
[21:35] <ttmrichter> It has an 8-bit register, a couple of 16-bit registers, and a 24-bit register.
[21:35] <ttmrichter> A simplistic "just store this value" is not a good thing.
[21:35] <ttmrichter> accumulator     @ a(A),   ra(_)  <=>   u8(A)   | ra(A).
[21:35] <ttmrichter>                   a(A),   ra(_)  <=> \+u8(A)   | fail.
[21:35] <ttmrichter> So I do that.
[21:36] <ttmrichter> a(+A) sets the accumulator value of register A iff the provided value fits in an unsigned integer width.
[21:37] <ttmrichter> The whole .... guard | action.  ... \+guard | fail. pattern is everywhere in my code.


useful cheatsheet

https://dtai.cs.kuleuven.be/CHR/files/CHR_cheatsheet.pdf

Falco Nogatz worked on this
http://www1.informatik.uni-wuerzburg.de/en/news/single/news/improving-deutsche-bahn-with-prolog/

https://github.com/fnogatz/CHR.js-website
 */

