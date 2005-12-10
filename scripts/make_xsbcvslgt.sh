#! /bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.26.2
##
## Copyright (c) 1998-2005 Paulo Moura.  All Rights Reserved.
## =================================================================

echo
echo "Creating a script named xsbcvslgt for running Logtalk with XSB..."
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
cp ../compiler/logtalk.pl logtalkcvs.P
echo ":- reconsult('$LOGTALKUSER/configs/xsbcvs.P')." > logtalkxsbcvs.P
echo ":- reconsult('$LOGTALKHOME/bin/logtalkcvs.P')." >> logtalkxsbcvs.P
echo ":- reconsult('$LOGTALKUSER/libpaths/libpaths.P')." >> logtalkxsbcvs.P
echo "#/bin/sh" > xsbcvslgt
echo "xsb -e \"reconsult('\$LOGTALKHOME/bin/logtalkxsbcvs.P').\"" >> xsbcvslgt
chmod a+x xsbcvslgt
ln -sf $LOGTALKHOME/bin/xsbcvslgt $prefix/bin/xsbcvslgt
echo "Done. A link to the script was been created in $prefix/bin."
echo "Users should define the environment variables LOGTALKHOME and"
echo "LOGTALKUSER in order to use the script."
echo
echo "Users must change the extension of files \$LOGTALKUSER/configs/xsb.config"
echo "and \$LOGTALKUSER/libpaths/libpaths.pl to '.P' before using the xsbcvslgt"
echo "script. In addition, users must edit the libpaths.P file to replace all"
echo "occurrences of the LOGTALKUSER environment variable by its value."
echo
