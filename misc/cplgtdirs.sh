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
	echo "The env variable LOGTALKHOME must be defined first!"
else
	mkdir -p $HOME/logtalk/examples
	mkdir -p $HOME/logtalk/library
	mkdir -p $HOME/logtalk/xml
	cp $LOGTALKHOME/compiler/libpaths_user_template.pl $HOME/logtalk/libpaths.pl
	cp -RL $LOGTALKHOME/examples $HOME/logtalk/
	cp -RL $LOGTALKHOME/library $HOME/logtalk/
	cp -RL $LOGTALKHOME/xml $HOME/logtalk/
	chmod -R u+w $HOME/logtalk
	echo "Finished copying Logtalk directories."
	echo
	echo "You may need to edit the ~/logtalk/libpaths.pl file to match your Prolog"
	echo "compiler and operating-system requirments or to add your own library paths."
	echo
fi
