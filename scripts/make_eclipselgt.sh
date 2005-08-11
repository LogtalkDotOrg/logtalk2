#! /bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.25.2
##
## Copyright (c) 1998-2005 Paulo Moura.  All Rights Reserved.
## =================================================================

echo
echo "Creating a script named eclipselgt for running Logtalk with ECLiPSe..."

if ! [ "$LOGTALKHOME" ]
then
	echo "The environment variable LOGTALKHOME must be defined first!"
	echo
	exit 1
fi

if ! [ -d "$LOGTALKHOME" ]; then
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
echo ":- pragma(system)." > logtalk_eclipse.pl
echo ":- pragma(nodebug)." >> logtalk_eclipse.pl
echo ":- ensure_loaded(library(toplevel))." >> logtalk_eclipse.pl
echo ":- include('\$LOGTALKHOME/compiler/logtalk.pl')." >> logtalk_eclipse.pl
echo ":- compile('\$LOGTALKUSER/configs/eclipseiso.config')." > logtalk_eclipse.rc
echo ":- compile('\$LOGTALKHOME/bin/logtalk_eclipse.pl')." >> logtalk_eclipse.rc
echo ":- compile('\$LOGTALKUSER/libpaths/libpaths.pl')." >> logtalk_eclipse.rc
echo "#/bin/sh" > eclipselgt
echo "eclipse -b \$LOGTALKHOME/bin/logtalk_eclipse.rc" >> eclipselgt
chmod a+x eclipselgt
ln -sf $LOGTALKHOME/bin/eclipselgt $prefix/bin/eclipselgt
echo "Done. A link to the script was been created in $prefix/bin."
echo "Users should define the environment variables LOGTALKHOME and"
echo "LOGTALKUSER in order to use the script."
echo
