#!/bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.29.6
##
## Copyright (c) 1998-2007 Paulo Moura.  All Rights Reserved.
## =================================================================

echo
echo "Creating a script named gplgt for running Logtalk with GNU Prolog..."
echo

if ! [ "$LOGTALKHOME" ]; then
	echo "The environment variable LOGTALKHOME should be defined first!"
	echo "Trying default Logtalk installation directories..."
	if [ -d "/usr/local/logtalk" ]; then
		LOGTALKHOME=/usr/local/logtalk
		echo "Using Logtalk installation at /usr/local/logtalk"
	elif [ -d "/opt/local/logtalk" ]; then
		LOGTALKHOME=/opt/local/logtalk
		echo "Using Logtalk installation at /opt/local/logtalk"
	elif [ -d "/opt/local/share/logtalk" ]; then
		LOGTALKHOME=/opt/local/share/logtalk
		echo "Using Logtalk installation at /opt/local/share/logtalk"
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

if [ -z "$1" ]; then
	prefix=/usr/local
else
	prefix="$1"
fi

if ! [ -d "$prefix" ]; then
	echo "Directory prefix does not exist!"
	echo
	exit 1
fi

cd "$LOGTALKHOME"
mkdir -p bin
cd bin
echo ":- built_in." > logtalk_gp.pl
cat ../compiler/logtalk.pl >> logtalk_gp.pl
echo "#/bin/sh" > gplgt
echo "gprolog --init-goal \"['\$LOGTALKUSER/configs/gnu.config', '\$LOGTALKHOME/bin/logtalk_gp.pl', '\$LOGTALKUSER/libpaths/libpaths.pl']\" \"\$@\"" >> gplgt
chmod a+x gplgt
ln -sf $LOGTALKHOME/bin/gplgt $prefix/bin/gplgt
echo "Done. A link to the script was been created in $prefix/bin."
echo
echo "Users should ensure that the environment variables LOGTALKHOME"
echo "and LOGTALKUSER are defined and then run the \"cplgtdirs\" script"
echo "once prior to using the gplgt script."
echo
