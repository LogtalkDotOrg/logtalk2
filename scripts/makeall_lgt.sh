#! /bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.25.0
##
## Copyright (c) 1998-2005 Paulo Moura.  All Rights Reserved.
## =================================================================

echo
echo "Creating scripts for running Logtalk with selected Prolog compilers..."
echo

if ! [ "$LOGTALKHOME" ]
then
	echo "The environment variable LOGTALKHOME must be defined first!"
	echo
	exit 1
else
	if [ -z "$1" ]; then
		prefix=/usr/local
	else
		prefix="$1"
	fi
	./make_ciaolgt.sh $prefix > /dev/null
	if [ $? ]; then
		echo "ciaolgt script created"
	fi
	./make_eclipselgt.sh $prefix > /dev/null
	if [ $? ]; then
		echo "eclipselgt script created"
	fi
	./make_gplgt.sh $prefix > /dev/null
	if [ $? ]; then
		echo "gplgt script created"
	fi
	./make_plclgt.sh $prefix > /dev/null
	if [ $? ]; then
		echo "plclgt script created"
	fi
	./make_sicstuslgt.sh $prefix > /dev/null
	if [ $? ]; then
		echo "sicstuslgt script created"
	fi
	./make_swilgt.sh $prefix > /dev/null
	if [ $? ]; then
		echo "swilgt script created"
	fi
	./make_xsblgt.sh $prefix > /dev/null
	if [ $? ]; then
		echo "xsblgt script created"
	fi
	./make_yaplgt.sh $prefix > /dev/null
	if [ $? ]; then
		echo "yaplgt script created"
	fi	
	echo
	echo "Done. Links to the created scripts can be found on $prefix/bin."
	echo "Make sure that the Prolog compilers are also available on your "
	echo "execution path.  Users should define the environment variables "
	echo "LOGTALKHOME and LOGTALKUSER in order to use the scripts."
	echo
fi
