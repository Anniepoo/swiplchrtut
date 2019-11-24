:- module(moduleb, [in_b/0]).

:- use_module(library(chr)).

:-chr_constraint in_b/0, private_b/0.

in_b ==> writeln('made in_b').

private_b ==> writeln('in private b').


