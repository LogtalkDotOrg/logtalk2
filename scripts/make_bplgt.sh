#!/bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.28.0
##
## Copyright (c) 1998-2006 Paulo Moura.  All Rights Reserved.
## =================================================================

echo
echo "Creating a script named bplgt for running Logtalk with B-Prolog ..."
echo

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
eval bp -g "set_prolog_flag(redefined, off), compile('\$LOGTALKHOME/compiler/logtalk.pl')."
mv $LOGTALKHOME/compiler/logtalk.pl.out .

echo "#/bin/sh" > bplgt
echo "bp -g  \"compile('\$LOGTALKUSER/configs/b.config'), load('\$LOGTALKUSER/configs/b.config'), load('\$LOGTALKHOME/bin/logtalk.pl'), compile('\$LOGTALKUSER/libpaths/libpaths.pl'), load('\$LOGTALKUSER/libpaths/libpaths.pl')\"" >> bplgt
chmod a+x bplgt
ln -sf $LOGTALKHOME/bin/bplgt $prefix/bin/bplgt
echo "Done. A link to the script was been created in $prefix/bin."
echo "Users must define the environment variables LOGTALKHOME and"
echo "LOGTALKUSER in order to use the script."
echo
