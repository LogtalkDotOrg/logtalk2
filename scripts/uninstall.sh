#!/bin/sh

## ================================================================
## Logtalk - Open source object-oriented logic programming language
## Release 2.43.2
## 
## Copyright (c) 1998-2011 Paulo Moura.        All Rights Reserved.
## Logtalk is free software.  You can redistribute it and/or modify
## it under the terms of the "Artistic License 2.0" as published by 
## The Perl Foundation. Consult the "LICENSE.txt" file for details.
## ================================================================

echo
echo "Uninstalling Logtalk 2.43.2 system-level files..."
echo

if ! [ "$LOGTALKHOME" ]; then
	echo "The environment variable LOGTALKHOME should be defined first!"
	echo "Trying default Logtalk installation directories..."
	if [ -d "/usr/local/share/logtalk" ]; then
		LOGTALKHOME=/usr/local/share/logtalk
		echo "Using Logtalk installation at \"/usr/local/share/logtalk\""
	elif [ -d "/usr/share/logtalk" ]; then
		LOGTALKHOME=/usr/share/logtalk
		echo "Using Logtalk installation at \"/usr/share/logtalk\""
	elif [ -d "/opt/local/share/logtalk" ]; then
		LOGTALKHOME=/opt/local/share/logtalk
		echo "Using Logtalk installation at \"/opt/local/share/logtalk\""
	elif [ -d "/opt/share/logtalk" ]; then
		LOGTALKHOME=/opt/share/logtalk
		echo "Using Logtalk installation at \"/opt/share/logtalk\""
	else
		echo "Unable to locate Logtalk installation directory!"
		echo
		exit 1
	fi
	elif ! [ -d "$LOGTALKHOME" ]; then
		echo "The environment variable LOGTALKHOME points to a non-existing directory!"
		echo "Its current value is: $LOGTALKHOME"
		echo "The variable must be set to your Logtalk installation directory!"
		echo
		exit 1
fi

cd $LOGTALKHOME/..
rm -rf lgt2432
rm -f logtalk
cd ../bin
rm -f bplgt
rm -f logtalk_user_setup
rm -f cxlgt
rm -f eclipselgt
rm -f gplgt
rm -f lgt2html
rm -f lgt2pdf
rm -f lgt2xml
rm -f lgt2txt
rm -f logtalk_version_select
rm -f logtalk_backend_select
rm -f lplgt
rm -f qplgt
rm -f sicstuslgt
rm -f swilgt
rm -f xsblgt
rm -f xsb64lgt
rm -f xsbmtlgt
rm -f xsbmt64lgt
rm -f yaplgt
cd ../share/man/man1
rm -f bplgt.1.gz
rm -f cxlgt.1.gz
rm -f eclipselgt.1.gz
rm -f lgt2html.1.gz
rm -f lgt2pdf.1.gz
rm -f lgt2txt.1.gz
rm -f lgt2xml.1.gz
rm -f logtalk_backend_select.1.gz
rm -f logtalk_user_setup.1.gz
rm -f logtalk_version_select.1.gz
rm -f lplgt.1.gz
rm -f qplgt.1.gz
rm -f sicstuslgt.1.gz
rm -f swilgt.1.gz
rm -f xsb64lgt.1.gz
rm -f xsblgt.1.gz
rm -f xsbmt64lgt.1.gz
rm -f xsbmtlgt.1.gz
rm -f yaplgt.1.gz


echo "Logtalk 2.43.2 system-level uninstall completed. For uninstalling user-level"
echo "Logtalk files simply delete the LOGTALKUSER directories."
echo
