#!/bin/sh

if ! [ $LOGTALKHOME ]
then
	echo "The environment variable LOGTALKHOME must be defined first!"
else

	XSL="$LOGTALKHOME/xml/lgtpdfa4.xsl"

	if [ -z "$1" ]; then
		output_dir="."
	else
		output_dir="$1"
	fi

	echo 
	echo This script converts all .xml files in the current directory to .pdf
	echo files applying the XSL-FO transformation defined in the file:
	echo "    "$XSL
	echo using the Apache FOP processor. It accepts as an optional parameter
	echo an output directory for writing the generated PDF files.

	cp $LOGTALKHOME/xml/logtalk.dtd .
	cp $LOGTALKHOME/xml/logtalk.xsd .

	echo
	echo converting XML files to PDF...

	for file in *.xml; do
		echo converting $file
		name="`expr "$file" : '\(.*\)\.[^./]*$' \| "$file"`"
		eval sh fop -q -xsl $XSL -xml $file -pdf $output_dir/$name.pdf
	done

	echo
	echo conversion done
	echo

	rm logtalk.dtd
	rm logtalk.xsd

fi
