#!/bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.22.0
##
## Copyright (c) 1998-2004 Paulo Moura.  All Rights Reserved.
## =================================================================

echo
echo "This script copies the Logtalk library, xml, and examples"
echo "directories to the user home directory (~/logtalk)."
echo

if ! [ $LOGTALKHOME ]
then
	echo "The environment variable LOGTALKHOME must be defined first!"
else
	if ! [ $LOGTALKUSER ]
	then
		LOGTALKUSER=$HOME/logtalk
		echo "After the script complishion, you must set the environment variable"
		echo "LOGTALKUSER pointing to $LOGTALKUSER"
	fi
	mkdir -p $LOGTALKUSER/configs
	mkdir -p $LOGTALKUSER/examples
	mkdir -p $LOGTALKUSER/library
	mkdir -p $LOGTALKUSER/xml
	cp -RL $LOGTALKHOME/configs $LOGTALKUSER/
	cp -RL $LOGTALKHOME/examples $LOGTALKUSER/
	cp -RL $LOGTALKHOME/libpaths $LOGTALKUSER/
	cp -RL $LOGTALKHOME/library $LOGTALKUSER/
	cp -RL $LOGTALKHOME/xml $LOGTALKUSER/
	chmod -R u+w $LOGTALKUSER
	ln -sf $LOGTALKHOME/BIBLIOGRAPHY $LOGTALKUSER/BIBLIOGRAPHY
	ln -sf $LOGTALKHOME/LICENSE $LOGTALKUSER/LICENSE
	ln -sf $LOGTALKHOME/QUICK_START $LOGTALKUSER/QUICK_START
	ln -sf $LOGTALKHOME/README $LOGTALKUSER/README
	ln -sf $LOGTALKHOME/RELEASE_NOTES $LOGTALKUSER/RELEASE_NOTES
	ln -sf $LOGTALKHOME/UPGRADING $LOGTALKUSER/UPGRADING
	ln -sf $LOGTALKHOME/manuals $LOGTALKUSER/manuals
	echo "Finished copying Logtalk directories."
	echo
	echo "You may need to edit the \$LOGTALKUSER/libpaths/libpaths.pl file to match"
	echo "your Prolog compiler and operating-system requirements or to add your own"
	echo "library paths."
	echo
fi
