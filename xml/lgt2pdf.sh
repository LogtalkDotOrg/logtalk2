#!/bin/bash

a4xsl="$LOGTALKHOME/xml/lgtpdfa4.xsl"
usxsl="$LOGTALKHOME/xml/lgtpdfus.xsl"

format=a4
# format=us

processor=fop
# processor=xep

directory="."

usage_help()
{
	echo 
	echo This script converts all XML files in the current directory to PDF files
	echo
	echo "Usage: $0 -f format -o directory -p processor"
	echo
	echo "Optional arguments:"
	echo "  -f paper format (either a4 or us; default is $format)"
	echo "  -o output directory for the PDF files (default is $directory)"
	echo "  -p XSL-FO processor (name of the executable; default is $processor)"
	echo "  -h help"
	echo
	exit 1
}

if ! [ $LOGTALKHOME ]
then
	echo "The environment variable LOGTALKHOME must be defined first!"
else

	while getopts "f:o:p:h" Option
	do
		case $Option in
			f) format="$OPTARG";;
			o) directory="$OPTARG";;
			p) processor="$OPTARG";;
			h) usage_help;;
			*) usage_help;;
		esac
	done

	if [ "$format" = "a4" ]
	then
		xsl=$a4xsl
	elif [ "$format" = "us" ]
	then
		xsl=$usxsl
	else
		echo unsupported paper format: $format
		echo
		exit 1
	fi

	cp $LOGTALKHOME/xml/logtalk.dtd .
	cp $LOGTALKHOME/xml/logtalk.xsd .

	echo
	echo converting XML files to PDF...

	for file in *.xml; do
		echo converting $file
		name="`expr "$file" : '\(.*\)\.[^./]*$' \| "$file"`"
		eval sh $processor -q -xsl $xsl -xml $file -pdf $directory/$name.pdf
	done

	echo
	echo conversion done
	echo

	rm logtalk.dtd
	rm logtalk.xsd

fi
