#!/bin/sh

echo
echo "This script copies the Logtalk library, xml, and"
echo "examples directories to the user home directory."
echo

if ! [ $LOGTALKHOME ]
then
	echo "The env variable LOGTALKHOME must be defined first!"
else
	mkdir -p $HOME/logtalk/examples
	mkdir -p $HOME/logtalk/library
	mkdir -p $HOME/logtalk/xml
	cp -RL $LOGTALKHOME/examples $HOME/logtalk/
	cp -RL $LOGTALKHOME/library $HOME/logtalk/
	cp -RL $LOGTALKHOME/xml $HOME/logtalk/
	echo "Finished copying Logtalk directories."
	echo
fi
