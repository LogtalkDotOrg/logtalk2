#!/bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.29.6
##
## Copyright (c) 1998-2007 Paulo Moura.  All Rights Reserved.
## =================================================================

echo
echo "Creating a script named cxlgt for running Logtalk with CxProlog..."
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
echo ":- set_prolog_flag(file_name_variables, true)." > logtalk_cx.pl
echo ":- silent_consult('\$LOGTALKUSER/configs/cx.config')." >> logtalk_cx.pl
echo ":- silent_consult('\$LOGTALKHOME/bin/logtalk_comp_cx.pl')." >> logtalk_cx.pl
echo ":- silent_consult('\$LOGTALKUSER/libpaths/libpaths.pl')." >> logtalk_cx.pl
echo "#!/bin/sh" > cxlgt
echo "cxprolog --goal silent_consult\(\"'\$LOGTALKHOME/bin/logtalk_cx.pl'\"\) \"\$@\"" >> cxlgt
chmod a+x cxlgt
ln -sf $LOGTALKHOME/bin/cxlgt $prefix/bin/cxlgt
echo "Done. A link to the script was been created in $prefix/bin."
echo
echo "Users should ensure that the environment variables LOGTALKHOME"
echo "and LOGTALKUSER are defined and then run the \"cplgtdirs\" script"
echo "once prior to using the cxlgt script."
echo
