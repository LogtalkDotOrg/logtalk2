#! /bin/sh

echo
echo "This script makes a script named gplgt for running Logtalk with GNU Prolog."
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
	cp ../configs/gnu.config gnu.pl
	echo ":- built_in." > logtalkgp.pl
	cat ../compiler/logtalk.pl >> logtalkgp.pl
	gplc -o gplgt gnu.pl logtalkgp.pl
	chmod a+x gplgt
	ln -sf $LOGTALKHOME/bin/gplgt /usr/local/bin/gplgt
	rm gnu.pl
	rm logtalkgp.pl
fi
