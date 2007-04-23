#/bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.29.6
##
## Copyright (c) 1998-2007 Paulo Moura.  All Rights Reserved.
## =================================================================

if ! [ "$LOGTALKHOME" ]; then
	echo "The environment variable LOGTALKHOME should be defined first!"
	echo "Trying default Logtalk installation directories..."
	if [ -d "/usr/local/share/logtalk" ]; then
		LOGTALKHOME=/usr/local/share/logtalk
		echo "Using Logtalk installation at /usr/local/share/logtalk"
	elif [ -d "/usr/share/logtalk" ]; then
		LOGTALKHOME=/usr/share/logtalk
		echo "Using Logtalk installation at /usr/share/logtalk"
	elif [ -d "/opt/local/share/logtalk" ]; then
		LOGTALKHOME=/opt/local/share/logtalk
		echo "Using Logtalk installation at /opt/local/share/logtalk"
	elif [ -d "/opt/share/logtalk" ]; then
		LOGTALKHOME=/opt/share/logtalk
		echo "Using Logtalk installation at /opt/share/logtalk"
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

if ! [ "$LOGTALKUSER" ]; then
	echo "The environment variable LOGTALKUSER should be defined first!"
	echo "Trying default Logtalk user installation directory..."
	if [ -d "$HOME/logtalk" ]; then
		LOGTALKUSER=$HOME/logtalk
		echo "Using Logtalk user directory at $HOME/logtalk"
	fi
elif ! [ -d "$LOGTALKUSER" ]; then
	echo "Cannot find \$LOGTALKUSER directory! Creating a new Logtalk user directory"
	echo "by running the \"cplgtdirs\" shell script:"
	cplgtdirs
fi

gprolog --init-goal "['$LOGTALKUSER/configs/gnu.config', '$LOGTALKHOME/integration/logtalk_gp.pl', '$LOGTALKUSER/libpaths/libpaths.pl']" "$@"
