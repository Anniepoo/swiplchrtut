Tutorial - CHR Constraint Handling Rules - Conclusion
=====================================================
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

Interesting Uses
----------------

There have been quite a few large systems built with CHR over the years.

link:https://www.securitease.com/[SecuritEase], often cited as a Prolog user, has various portions of its' system in CHR. Inside SecuritEase CHR 
link:https://dtai.cs.kuleuven.be/projects/CHR/papers/draft_chr_survey.pdf[is used to]
:

1.  implement the logic to recognize advantageous market conditions to auto-
matically place orders in equity markets,
2.  translate high-level queries to SQL,
3.  describe complex relationships between mutually dependent fields on user
input screens, and calculating the consequences of user input actions, and
4.  realize a Financial Information eXchange (FIX) server.

Some Prolog folks, Falco Nogatz and Christian Hieke, won first place in a 
link:http://www1.informatik.uni-wuerzburg.de/en/news/single/news/improving-deutsche-bahn-with-prolog/[Deutsche Bahn Open Data Hackathon]
with a system that analyzed the fares of the German National Railways and discovered several places where it was cheaper to buy a ticket part way, then another ticket on the same train to one's final destination.

Some Prolog folks enter the large 
link:http://ldjam.com[Ludum Dare] game jam when it comes around. The October 2019 entry used CHR to recognize hand drawn houses as part of a game.

Falco Nogatz has an interesting 
link:https://github.com/fnogatz/CHR-Linear-Equation-Solver[linear equation solver]
.


Conclusion
-----------

CHR is a powerful addition to the logic programmer's toolkit. I hope you'll find it useful.

I hope you've enjoyed this tutorial. If you notice any mistakes, or want to suggest improvements, or just are totally stumped, email annie (at) swi-prolog (dot) org and let me know.

You can often find me as Anniepoo on ##prolog channel on freenode.net IRC.

Massive Thanks
--------------

This tutorial, like most of my tutorials, came from my own desire to understand the system.
So I've been somewhere between a constant questioner and an outright pest to many in the CHR
community for the past few months.

Thanks to Thom Frühwirth for the CHR library and for answering questions on the CHR list and email.

Alan Baljeu gave much patient coaching on the CHR list, and spent an evening on video call explaining CHR.

Falco Nogatz provided yet more explanations, as well as many contributions to the CHR ecosystem.

Tom Schrijvers slides from ICLP are a great resource. I've also stolen a few examples in this tutorial from his work.

Michael Richter and I puzzled out bits of this together. In particular Michael figured out how the right hand side works.

Thanks to Gergö Barany for a pleasant afternoon in Vienna spent puzzling out bits of CHR.


