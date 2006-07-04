#!/bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.28.0
##
## Copyright (c) 1998-2006 Paulo Moura.  All Rights Reserved.
## =================================================================

echo
echo "Creating a script named swilgt for running Logtalk with SWI-Prolog..."
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
echo ":- set_prolog_flag(generate_debug_info, false)." > logtalk_swi.pl
echo ":- system_module." >> logtalk_swi.pl
cat ../compiler/logtalk.pl >> logtalk_swi.pl
echo ":- consult('\$LOGTALKUSER/configs/swi.config')." > logtalk_swi.rc
echo ":- consult('\$LOGTALKUSER/configs/swihook.pl')." >> logtalk_swi.rc
echo ":- consult('\$LOGTALKHOME/bin/logtalk_swi.pl')." >> logtalk_swi.rc
echo ":- consult('\$LOGTALKUSER/libpaths/libpaths.pl')." >> logtalk_swi.rc
echo "#/bin/sh" > swilgt
case $( uname -s ) in
	Darwin	) echo "swipl -f \$LOGTALKHOME/bin/logtalk_swi.rc" >> swilgt;;
	*		) echo "pl -f \$LOGTALKHOME/bin/logtalk_swi.rc" >> swilgt;;
esac
chmod a+x swilgt
ln -sf $LOGTALKHOME/bin/swilgt $prefix/bin/swilgt
echo "Done. A link to the script was been created in $prefix/bin."
echo "Users must define the environment variables LOGTALKHOME and"
echo "LOGTALKUSER in order to use the script."
echo
echo "Users must run the batch script cplgtdirs before using the"
echo "swilgt script."
echo
