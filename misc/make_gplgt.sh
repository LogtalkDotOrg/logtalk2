#! /bin/sh

echo
echo "Making a script named gplgt for running Logtalk with GNU Prolog..."

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
	cp ../configs/gnu.config gnu.pl
	echo ":- built_in." > logtalkgp.pl
	cat ../compiler/logtalk.pl >> logtalkgp.pl
	gplc -o gplgt gnu.pl logtalkgp.pl
	chmod a+x gplgt
	ln -sf $LOGTALKHOME/bin/gplgt $prefix/bin/gplgt
	rm gnu.pl
	rm logtalkgp.pl
	echo "Done. A link to the script was been created in $prefix/bin."
	echo
fi
