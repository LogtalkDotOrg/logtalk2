#! /bin/sh

echo
echo "This script makes a script named yaplgt for running Logtalk with YAP."
echo

if ! [ $LOGTALKHOME ]
then
	echo "The env variable LOGTALKHOME must be defined first!"
else
	cd $LOGTALKHOME
	if ! [ -d bin ]
	then
		mkdir bin
	fi
	cd bin
	echo ":- reconsult('$LOGTALKHOME/configs/yap.config')." > logtalkyap.rc
	echo ":- reconsult('$LOGTALKHOME/compiler/logtalk.pl')." >> logtalkyap.rc

	echo "#/bin/sh" > yaplgt
	echo "yap -l $LOGTALKHOME/bin/logtalkyap.rc" >> yaplgt
	chmod a+x yaplgt
	ln -sf $LOGTALKHOME/bin/yaplgt /usr/local/bin/yaplgt
fi
