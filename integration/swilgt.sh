#/bin/sh

## ================================================================
## Logtalk - Open source object-oriented logic programming language
## Release 2.31.1
##
## Copyright (c) 1998-2008 Paulo Moura.  All Rights Reserved.
## ================================================================

if ! [ "$LOGTALKHOME" ]; then
	echo "The environment variable LOGTALKHOME should be defined first, pointing"
	echo "to your Logtalk installation directory!"
	echo "Trying the default locations for the Logtalk installation..."
	if [ -d "/usr/local/share/logtalk" ]; then
		LOGTALKHOME=/usr/local/share/logtalk
		echo "... using Logtalk installation found at /usr/local/share/logtalk"
	elif [ -d "/usr/share/logtalk" ]; then
		LOGTALKHOME=/usr/share/logtalk
		echo "... using Logtalk installation found at /usr/share/logtalk"
	elif [ -d "/opt/local/share/logtalk" ]; then
		LOGTALKHOME=/opt/local/share/logtalk
		echo "... using Logtalk installation found at /opt/local/share/logtalk"
	elif [ -d "/opt/share/logtalk" ]; then
		LOGTALKHOME=/opt/share/logtalk
		echo "... using Logtalk installation found at /opt/share/logtalk"
	else
		echo "... unable to locate Logtalk installation directory!"
		echo
		exit 1
	fi
	echo
elif ! [ -d "$LOGTALKHOME" ]; then
	echo "The environment variable LOGTALKHOME points to a non-existing directory!"
	echo "Its current value is: $LOGTALKHOME"
	echo "The variable must be set to your Logtalk installation directory!"
	echo
	exit 1
fi
export LOGTALKHOME

if ! [ "$LOGTALKUSER" ]; then
	echo "The environment variable LOGTALKUSER should be defined first, pointing"
	echo "to your Logtalk user directory!"
	echo "Trying the default location for the Logtalk user directory..."
	export LOGTALKUSER=$HOME/logtalk
	if [ -d "$LOGTALKUSER" ]; then		
		echo "... using Logtalk user directory found at $LOGTALKUSER"
	else
		echo "... Logtalk user directory not found at default location. Creating a"
		echo "new Logtalk user directory by running the \"cplgtdirs\" shell script:"
		cplgtdirs
	fi
elif ! [ -d "$LOGTALKUSER" ]; then
	echo "Cannot find \$LOGTALKUSER directory! Creating a new Logtalk user directory"
	echo "by running the \"cplgtdirs\" shell script:"
	cplgtdirs
fi
echo

if pl -t halt 2>&1 | grep "SWI-Prolog"; then
	exec pl -f "$LOGTALKHOME/integration/logtalk_swi.pl" "$@"
elif swipl -t halt 2>&1 | grep "SWI-Prolog"; then
	exec swipl -f "$LOGTALKHOME/integration/logtalk_swi.pl" "$@"
else case $( uname -s ) in
	Darwin	) exec swipl -f "$LOGTALKHOME/integration/logtalk_swi.pl" "$@";;
	*		) exec pl -f "$LOGTALKHOME/integration/logtalk_swi.pl" "$@";;
esac
fi
