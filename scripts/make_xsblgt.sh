#! /bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.22.2
##
## Copyright (c) 1998-2004 Paulo Moura.  All Rights Reserved.
## =================================================================

echo
echo "Creating a script named xsblgt for running Logtalk with XSB..."

if ! [ $LOGTALKHOME ]
then
	echo "The environment variable LOGTALKHOME must be defined first!"
else
	cd $LOGTALKHOME
	if [ -z "$1" ]; then
		prefix=/usr/local
	else
		prefix="$1"
	fi
	mkdir -p bin
	cd bin
	cp ../compiler/logtalk.pl logtalk.P
	sed 's/^..lgt_current_object_.[(]user.*[)]/:- assertz(&)/' logtalk.P > temp1
	sed 's/^..lgt_current_object_.[(]debugger.*[)]/:- assertz(&)/' temp1 > temp2
	sed 's/^..lgt_dbg_leashing_.[(].*[)]/:- assertz(&)/g' temp2 > logtalk.P
	rm temp1
	rm temp2
	echo ":- reconsult('$LOGTALKUSER/configs/xsb.P')." > logtalkxsb.P
	echo ":- reconsult('$LOGTALKHOME/bin/logtalk.P')." >> logtalkxsb.P
	echo ":- reconsult('$LOGTALKUSER/libpaths/libpaths.P')." >> logtalkxsb.P
	echo "#/bin/sh" > xsblgt
	echo "xsb -e \"reconsult('\$LOGTALKHOME/bin/logtalkxsb.P').\"" >> xsblgt
	chmod a+x xsblgt
	ln -sf $LOGTALKHOME/bin/xsblgt $prefix/bin/xsblgt
	echo "Done. A link to the script was been created in $prefix/bin."
	echo "Users should define the environment variables LOGTALKHOME and"
	echo "LOGTALKUSER in order to use the script."
	echo
	echo "In addition, each user will need to change the extension of the files"
	echo "\$LOGTALKUSER/configs/xsb.config and \$LOGTALKUSER/libpaths/libpaths.pl"
	echo "to '.P' before using the xsblgt script."
	echo
fi
