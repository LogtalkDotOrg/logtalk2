#!/bin/sh

## ================================================================
## Logtalk - Open source object-oriented logic programming language
## Release 2.39.0
## 
## Copyright (c) 1998-2010 Paulo Moura.        All Rights Reserved.
## Logtalk is free software.  You can redistribute it and/or modify
## it under the terms of the "Artistic License 2.0" as published by 
## The Perl Foundation. Consult the "LICENSE.txt" file for details.
## ================================================================

echo
echo "Uninstalling Logtalk 2.39.0 system-level files..."
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
rm -rf lgt2390
rm -f logtalk
cd ../bin
rm -f bplgt
rm -f ciaolgt
rm -f cplgtdirs
rm -f cxlgt
rm -f eclipselgt
rm -f gplgt
rm -f lgt2html
rm -f lgt2pdf
rm -f lgt2xml
rm -f lgt2txt
rm -f logtalk_select
rm -f plclgt
rm -f qplgt
rm -f quintuslgt
rm -f sicstuslgt
rm -f swilgt
rm -f xsblgt
rm -f xsb64lgt
rm -f xsbmtlgt
rm -f xsbmt64lgt
rm -f yaplgt

echo "Logtalk 2.39.0 system-level uninstall completed. For uninstalling user-level"
echo "Logtalk files simply delete the LOGTALKUSER directories."
echo
