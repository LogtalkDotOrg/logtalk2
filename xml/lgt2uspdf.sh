#!/bin/sh

if ! [ $LOGTALKHOME ]
then
	echo "The environment variable LOGTALKHOME must be defined first!"
else

	XSLT="$LOGTALKHOME/xml/lgtpdfus.xsl"

	if [ -z "$1" ]; then
		output_dir="."
	else
		output_dir="$1"
	fi

	echo 
	echo This script converts all .xml files in the current directory to .pdf
	echo files applying the XSLT transformation defined in the $XSLT file
	echo using the Apache FOP processor

	cp $LOGTALKHOME/xml/logtalk.dtd .
	cp $LOGTALKHOME/xml/logtalk.xsd .

	echo
	echo converting XML files to PDF...

	for file in *.xml; do
		echo converting $file
		name="`expr "$file" : '\(.*\)\.[^./]*$' \| "$file"`"
		eval sh fop -q -xsl $XSLT -xml $file -pdf $output_dir/$name.pdf
	done

	echo
	echo conversion done
	echo

	rm logtalk.dtd
	rm logtalk.xsd

fi
