# SWI-Prolog CHR tutorial

This is part of the tutorials served at [Pathways tutorials](http://www.pathwayslms.com/swipltuts/).

This is a tutorial that teaches Thom Frühwirth's CHR system that comes as part of the standard **SWI-Prolog
distribution.

## Install

Install asciidoctor

make a directory /etc/asciidoc/themes/pathways

copy the contents of the pathways directory under the project root into this dir


## Building

to rebuild the web pages, compile all .md files with asciidoc

````
asciidoc -a stylesheet=themes/pathways/style.css -a stylesdir=themes/pathways/ final.md >final.html
````

and serve.




