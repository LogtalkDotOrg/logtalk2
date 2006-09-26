#!/bin/bash

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.28.0
##
## Copyright (c) 1998-2006 Paulo Moura.  All Rights Reserved.
## =================================================================

css2xslfo=/Applications/XML/CSSToXSLFO/css2xslfo1_3_3.jar

directory="."

processor=fop
# processor=xep
# processor=xinc

usage_help()
{
	echo 
	echo "This script converts all Logtalk manual HTML files into PDF files"
	echo
	echo "Usage:"
	echo "  $0 -d directory -p processor"
	echo "  $0 -h"
	echo
	echo "Optional arguments:"
	echo "  -d output directory for the generated files (default is $directory)"
	echo "  -p XSL-FO processor (either fop, xep, or xinc; default is $processor)"
	echo "  -h help"
	echo
	exit 1
}

while getopts "f:d:i:t:p:h" Option
do
	case $Option in
		d) d_arg="$OPTARG";;
		p) p_arg="$OPTARG";;
		h) usage_help;;
		*) usage_help;;
	esac
done

if [[ "$d_arg" != "" && ! -d "$d_arg" ]]
then
	echo "Error! directory does not exists: $d_arg"
	usage_help
	exit 1
elif [ "$d_arg" != "" ]
then
	directory=$d_arg
fi

if [[ "$p_arg" != "" && "$p_arg" != "fop" && "$p_arg" != "xep" && "$p_arg" != "xinc" ]]
then
	echo "Error! Unsupported XSL-FO processor: $p_arg"
	usage_help
	exit 1
elif [ "$p_arg" != "" ]
then
	processor=$p_arg
fi

if [[ `(ls *.html | wc -l) 2> /dev/null` -gt 0 ]]
then
	echo
	echo "converting HTML files..."
	for file in *.html; do
		echo "  converting $file"
		name="`expr "$file" : '\(.*\)\.[^./]*$' \| "$file"`"
		java -jar $css2xslfo "$file" -fo "$name.fo"
		eval $processor -fo \"$name.fo\" -pdf \"$directory\"/\"$name.pdf\"
		rm $name.fo
	done
	echo "conversion done"
	echo
else
	echo
	echo "No HTML files exist in the current directory!"
	echo
fi

exit 0
