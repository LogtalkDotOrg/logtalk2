#!/bin/bash

## ================================================================
## Logtalk - Open source object-oriented logic programming language
## Release 2.41.1
## 
## Copyright (c) 1998-2010 Paulo Moura.        All Rights Reserved.
## Logtalk is free software.  You can redistribute it and/or modify
## it under the terms of the "Artistic License 2.0" as published by 
## The Perl Foundation. Consult the "LICENSE.txt" file for details.
## ================================================================


print_version() {
	echo "Current `basename $0` version:"
	echo "  0.3"
	exit 0
}


list_backends() {
    echo "Available back-end Prolog compilers:"
	if [ -e `which bplgt` ]  && [ "`which bp`" != "" ] ; then
		echo -n "  bplgt"
	fi
	if [ -e `which cxlgt` ]  && [ "`which cxprolog`" != "" ] ; then
		echo -n "  cxlgt"
	fi
	if [ -e `which eclipselgt` ]  && [ "`which eclipse`" != "" ] ; then
		echo -n "  eclipselgt"
	fi
	if [ -e `which qplgt` ]  && [ "`which qp`" != "" ] ; then
		echo -n "  qplgt"
	fi
	if [ -e `which sicstuslgt` ]  && [ "`which sicstus`" != "" ] ; then
		echo -n "  sicstuslgt"
	fi
	if [ -e `which swilgt` ]  && [ "`which swipl`" != "" ] ; then
		echo -n "  swilgt"
	fi
	if [ -e `which xsblgt` ]  && [ "`which xsb`" != "" ] ; then
		echo -n "  xsblgt"
	fi
	if [ -e `which xsb64lgt` ]  && [ "`which xsb-bits64`" != "" ] ; then
		echo -n "  xsb64lgt"
	fi
	if [ -e `which xsbmtlgt` ]  && [ "`which xsb-mt`" != "" ] ; then
		echo -n "  xsbmtlgt"
	fi
	if [ -e `which xsbmt64lgt` ]  && [ "`which xsb-bits64-mt`" != "" ] ; then
		echo -n "  xsbmt64lgt"
	fi
	if [ -e `which yaplgt` ]  && [ "`which yap`" != "" ] ; then
		echo -n "  yaplgt"
	fi
	echo
	exit 0
}


show_selected() {
    echo "Current Prolog integration script:"
    if [ -e `which logtalk` ] && [ "`which logtalk`" != "" ] ; then
		echo -n "  "
		readlink `which logtalk`
    else
        echo "  none"
    fi
	exit 0
}


usage_help() {
	echo 
	echo "This script allows the definition of a default back-end Prolog compiler by"
	echo "creating a symbolic link, \"logtalk\", to the corresponding integration script."
	echo
	echo "Usage:"
	echo "  `basename $0` [-vlsh] integration-script"
	echo
	echo "Optional arguments:"
	echo "  -v print script version"
	echo "  -l list available Prolog integration scripts"
	echo "  -s show the currently selected Prolog integration script"
	echo "  -h help"
	echo
	exit 0
}


valid_backend() {
	if [ "$1" == "bplgt" ] && [ -e `which bplgt` ]  && [ "`which bp`" != "" ] ; then
		return 0
	elif [ "$1" == "cxlgt" ] && [ -e `which cxlgt` ]  && [ "`which cxprolog`" != "" ] ; then
		return 0
	elif [ "$1" == "eclipselgt" ] && [ -e `which eclipselgt` ]  && [ "`which eclipse`" != "" ] ; then
		return 0
	elif [ "$1" == "qplgt" ] && [ -e `which qplgt` ]  && [ "`which qp`" != "" ] ; then
		return 0
	elif [ "$1" == "sicstuslgt" ] && [ -e `which sicstuslgt` ]  && [ "`which sicstus`" != "" ] ; then
		return 0
	elif [ "$1" == "swilgt" ] && [ -e `which swilgt` ]  && [ "`which swipl`" != "" ] ; then
		return 0
	elif [ "$1" == "xsblgt" ] && [ -e `which xsblgt` ]  && [ "`which xsb`" != "" ] ; then
		return 0
	elif [ "$1" == "xsb64lgt" ] && [ -e `which xsb64lgt` ]  && [ "`which xsb-bits64`" != "" ] ; then
		return 0
	elif [ "$1" == "xsbmtlgt" ] && [ -e `which xsbmtlgt` ]  && [ "`which xsb-mt`" != "" ] ; then
		return 0
	elif [ "$1" == "xsbmt64lgt" ] && [ -e `which xsbmt64lgt` ]  && [ "`which xsb-bits64-mt`" != "" ] ; then
		return 0
	elif [ "$1" == "yaplgt" ] && [ -e `which yaplgt` ]  && [ "`which yap`" != "" ] ; then
		return 0
	else
		return 1
	fi
}


switch_backend() {
	valid_backend $1
	if [ 0 != ${?} ]; then
    	echo "Invalid Prolog integration script: $1"
    	exit 1
	else
		cd $(dirname `which $1`)
		rm -f logtalk
		ln -sf $1 logtalk
		error=$?
		if [ 0 != $error ]; then
			echo "An error occurred when switching the default Prolog integration script!"
			echo "Check that you are executing this script with the necessary permissions."
			exit 1
    	else
			echo "Switched to the Prolog integration script:"
			echo "  $1"
			exit 0
		fi
	fi
}


while getopts "vlsh" option
do
	case $option in
		v) print_version;;
		l) list_backends;;
		s) show_selected;;
		h) usage_help;;
		*) usage_help;;
	esac
done


if [ "$1" == "" ]; then
	usage_help
else
	switch_backend $1
	error=$?
	if [ 0 != $error ]; then
		echo "An error occurred when switching to the $1 Prolog integration script!"
		exit 1
	fi
	exit 0
fi
