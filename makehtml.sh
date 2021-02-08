#!/bin/sh
asciidoc -a stylesheet=themes/pathways/style.css -a stylesdir=themes/pathways/ index.adoc >index.html
asciidoc -a stylesheet=themes/pathways/style.css -a stylesdir=themes/pathways/ basics.adoc >basics.html
asciidoc -a stylesheet=themes/pathways/style.css -a stylesdir=themes/pathways/ constraintsystems.adoc >constraintsystems.html
asciidoc -a stylesheet=themes/pathways/style.css -a stylesdir=themes/pathways/ advanced.adoc >advanced.html
asciidoc -a stylesheet=themes/pathways/style.css -a stylesdir=themes/pathways/ final.adoc >final.html
asciidoc -a stylesheet=themes/pathways/style.css -a stylesdir=themes/pathways/ examples.adoc >examples.html

