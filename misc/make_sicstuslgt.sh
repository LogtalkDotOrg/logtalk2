#! /bin/sh

echo
echo "Making a script named sicstuslgt for running Logtalk with SICStus Prolog..."

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
	if ! [ -d bin ]
	then
		mkdir bin
	fi
	cd bin
	echo ":- compile('\$LOGTALKHOME/configs/sicstus.config')." > logtalksicstus.rc
	echo ":- compile('\$LOGTALKHOME/compiler/logtalk.pl')." >> logtalksicstus.rc

	echo "#/bin/sh" > sicstuslgt
	echo "sicstus -l \$LOGTALKHOME/bin/logtalksicstus.rc" >> sicstuslgt
	chmod a+x sicstuslgt
	ln -sf $LOGTALKHOME/bin/sicstuslgt $prefix/bin/sicstuslgt
	echo "Done. A link to the script was been created in $prefix/bin."
	echo
fi
