#! /bin/sh

echo
echo "This script makes a script named swilgt for running Logtalk with SWI-Prolog."
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
	echo ":- system_module." > logtalkswi.pl
	cat ../compiler/logtalk.pl >> logtalkswi.pl
	echo ":- consult('$LOGTALKHOME/configs/swi.config')." > logtalkswi.rc
	echo ":- consult('$LOGTALKHOME/configs/swihook.pl')." >> logtalkswi.rc
	echo ":- consult('$LOGTALKHOME/bin/logtalkswi.pl')." >> logtalkswi.rc

	echo "#/bin/sh" > swilgt
	echo "swipl -f $LOGTALKHOME/bin/logtalkswi.rc" >> swilgt
	chmod a+x swilgt
	ln -sf $LOGTALKHOME/bin/swilgt /usr/local/bin/swilgt
	echo "A link to the script was been created in /usr/local/bin."
	echo
fi
