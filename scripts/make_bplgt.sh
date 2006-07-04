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
eval bp -g "set_prolog_flag(redefined, off), compile('compiler/logtalk.pl')."
mv compiler/logtalk.pl.out bin

echo ":- set_prolog_flag(redefined, off)." > logtalk_bp.pl
echo ":- chdir('$LOGTALKUSER')." >> logtalk_bp.pl
echo ":- compile('configs/b.config')." >> logtalk_bp.pl
echo ":- load('configs/b.config')." >> logtalk_bp.pl
echo ":- chdir('$LOGTALKHOME')." >> logtalk_bp.pl
echo ":- load('bin/logtalk.pl')." >> logtalk_bp.pl
echo ":- chdir('$LOGTALKUSER')." >> logtalk_bp.pl
echo ":- compile('libpaths/libpaths.pl')." >> logtalk_bp.pl
echo ":- load('libpaths/libpaths.pl')." >> logtalk_bp.pl

echo "#/bin/sh" > bplgt
echo "bp -g  \"chdir('$LOGTALKHOME/bin'), consult('logtalk_bp.pl')\"" >> bplgt
chmod a+x bplgt
ln -sf $LOGTALKHOME/bin/bplgt $prefix/bin/bplgt
echo "Done. A link to the script was been created in $prefix/bin."
echo "Users must define the environment variables LOGTALKHOME and"
echo "LOGTALKUSER in order to use the script."
echo
echo "Users must run the batch script cplgtdirs before using the"
echo "bplgt script."
echo
