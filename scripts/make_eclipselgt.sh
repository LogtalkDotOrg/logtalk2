#! /bin/sh

echo
echo "Making a script named eclipselgt for running Logtalk with ECLiPSe..."

if ! [ $LOGTALKHOME ]
then
	echo "The env variable LOGTALKHOME must be defined first!"
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
	echo ":- pragma(system)." > logtalkeclipse.pl
	echo ":- pragma(nodebug)." >> logtalkeclipse.pl
	echo ":- ensure_loaded(library(toplevel))." >> logtalkeclipse.pl
	echo ":- include('\$LOGTALKHOME/compiler/logtalk.pl')." >> logtalkeclipse.pl
	echo ":- compile('\$LOGTALKHOME/configs/eclipseiso.config')." > logtalkeclipse.rc
	echo ":- compile('\$LOGTALKHOME/bin/logtalkeclipse.pl')." >> logtalkeclipse.rc
	echo "#/bin/sh" > eclipselgt
	echo "eclipse -b \$LOGTALKHOME/bin/logtalkeclipse.rc" >> eclipselgt
	chmod a+x eclipselgt
	ln -sf $LOGTALKHOME/bin/eclipselgt $prefix/bin/eclipselgt
	echo "Done. A link to the script was been created in $prefix/bin."
	echo
fi
