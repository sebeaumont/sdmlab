# makefile for docs and stuff

doc:	README.pdf README.html

README.html:	README.md
	pandoc --filter pandoc-citeproc --bibliography=../doc/sdmlib/Bibliography.bib README.md -s -o README.html

README.pdf:	README.md
	pandoc --filter pandoc-citeproc --bibliography=../doc/sdmlib/Bibliography.bib README.md -s -o README.pdf
