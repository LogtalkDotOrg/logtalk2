#! /bin/sh

echo
echo "This script makes a script named qplgt for running Logtalk with Qu-Prolog."
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
	find . -name "*.lgt" -exec perl -pi -e "s/version is (\d)\.(\d)/version is '\1\.\2'/" {} \;
	cd configs
	cp qu.config qu.ql
	echo "fcompile('qu.ql', [assemble_only(true)]), load(qu). \
chdir('../compiler/'), fcompile('logtalk.pl', [assemble_only(true), string_table(256)]), load(logtalk)." | qp -s 2048 -d 1024 -h 2000
	qc -c qphook.ql
	cd ../bin
	qc -s 2048 -d 1024 -h 2000 -o qplgt ../configs/qphook.qo ../configs/qu.qo ../compiler/logtalk.qo
	chmod a+x qplgt
	ln -sf $LOGTALKHOME/bin/qplgt /usr/local/bin/qplgt
	rm ../configs/qu.ql
	rm ../configs/qphook.qo
	rm ../configs/qu.qo
	rm ../compiler/logtalk.qo
fi
