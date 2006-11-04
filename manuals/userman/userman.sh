#!/bin/bash

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.28.2
##
## Copyright (c) 1998-2006 Paulo Moura.  All Rights Reserved.
## =================================================================

css2xslfo=/Applications/XML/CSSToXSLFO/css2xslfo1_3_3.jar

rm -f userman.fo userman.html
rm -fr *.section
eval xsltproc -o index.section userman.xsl index.html
eval xsltproc -o features.section userman.xsl features.html
eval xsltproc -o nomenclature.section userman.xsl nomenclature.html
eval xsltproc -o messages.section userman.xsl messages.html
eval xsltproc -o objects.section userman.xsl objects.html
eval xsltproc -o protocols.section userman.xsl protocols.html
eval xsltproc -o categories.section userman.xsl categories.html
eval xsltproc -o predicates.section userman.xsl predicates.html
eval xsltproc -o inheritance.section userman.xsl inheritance.html
eval xsltproc -o events.section userman.xsl events.html
eval xsltproc -o threads.section userman.xsl threads.html
eval xsltproc -o errors.section userman.xsl errors.html
eval xsltproc -o documenting.section userman.xsl documenting.html
eval xsltproc -o installing.section userman.xsl installing.html
eval xsltproc -o running.section userman.xsl running.html
eval xsltproc -o programming.section userman.xsl programming.html

cat -s \
	userman.header \
	index.section \
	userman.body \
	features.section nomenclature.section messages.section \
	objects.section protocols.section categories.section predicates.section \
	inheritance.section events.section threads.section errors.section \
	documenting.section installing.section running.section programming.section \
	userman.footer \
	> userman.html

java -jar $css2xslfo userman.html -fo userman.fo
eval xep -fo userman.fo -pdf userman.pdf
rm userman.fo userman.html
rm -fr *.section
