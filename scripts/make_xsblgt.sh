#!/bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.28.0
##
## Copyright (c) 1998-2006 Paulo Moura.  All Rights Reserved.
## =================================================================

echo
echo "Creating a script named xsblgt for running Logtalk with XSB..."
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
echo ":- reconsult('$LOGTALKUSER/configs/xsb.pl')." > logtalkxsb.pl
echo ":- reconsult('$LOGTALKHOME/compiler/logtalk.pl')." >> logtalkxsb.pl
echo ":- reconsult('$LOGTALKUSER/libpaths/libpaths_no_env_var.pl')." >> logtalkxsb.pl
echo "#/bin/sh" > xsblgt
echo "xsb -e \"reconsult('\$LOGTALKHOME/bin/logtalkxsb.pl').\"" >> xsblgt
chmod a+x xsblgt
ln -sf $LOGTALKHOME/bin/xsblgt $prefix/bin/xsblgt
echo "Done. A link to the script was been created in $prefix/bin."
echo "Users must define the environment variables LOGTALKHOME and"
echo "LOGTALKUSER in order to use the script. Users must run the"
echo "the cplgtdirs script before using the xsblgt script."
echo
echo "The first call to the script must be made as root or using sudo."
echo
