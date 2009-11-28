#!/bin/bash

## ================================================================
## Logtalk - Open source object-oriented logic programming language
## Release 2.38.0
## 
## Copyright (c) 1998-2009 Paulo Moura.        All Rights Reserved.
## Logtalk is free software.  You can redistribute it and/or modify
## it under the terms of the "Artistic License 2.0" as published by 
## The Perl Foundation. Consult the "LICENSE.txt" file for details.
## ================================================================


print_version() {
	echo "`basename $0` 0.4"
	exit 1
}


list_versions() {
    echo -n "Available versions: "
	if [ `(ls -d "$prefix"/lgt* | wc -l) 2> /dev/null` -gt 0 ]; then
		for path in $(ls -d "$prefix"/lgt*); do
			file=`basename $path`
			if [ $file \> "lgt2351" ]; then
				echo -n "$file "
			fi
		done
		echo
	else
		echo "none"
	fi
	exit 1
}


show_selected() {
    echo -n "Current version: "
    if [ -e "$LOGTALKHOME" ]; then
		readlink "$LOGTALKHOME"
    else
        echo "none"
    fi
	exit 1
}


usage_help() {
	echo 
	echo "This script allows switching between installed Logtalk versions"
	echo
	echo "Usage:"
	echo "  `basename $0` [-vlsh] version"
	echo
	echo "Optional arguments:"
	echo "  -v print version of `basename $0`"
	echo "  -l list available versions"
	echo "  -s show the currently selected version"
	echo "  -h help"
	echo
	exit 1
}


valid_version() {
	if [ `(ls -d "$prefix"/lgt* | wc -l) 2> /dev/null` -gt 0 ]; then
	    for path in $(ls -d "$prefix"/lgt*); do
			version=`basename $path`
	        if [ $1 == $version -a $1 \> "lgt2351" ]; then
	            return 0
	        fi
	    done
	fi
	return 1
}


switch_version() {
	valid_version $1
	if [ 0 != ${?} ]; then
    	echo "Invalid version: $1"
    	exit 1
	else
		cd "$prefix"
		rm -f logtalk
		ln -sf $1 logtalk
    	echo "Switched to version: $1"
		exit 0
	fi
}


if ! [ "$LOGTALKHOME" ]; then
	echo "The environment variable LOGTALKHOME should be defined first, pointing"
	echo "to your Logtalk installation directory!"
	echo "Trying the default locations for the Logtalk installation..."
	if [ -d "/usr/local/share/logtalk" ]; then
		LOGTALKHOME=/usr/local/share/logtalk
		echo "... using Logtalk installation found at /usr/local/share/logtalk"
	elif [ -d "/usr/share/logtalk" ]; then
		LOGTALKHOME=/usr/share/logtalk
		echo "... using Logtalk installation found at /usr/share/logtalk"
	elif [ -d "/opt/local/share/logtalk" ]; then
		LOGTALKHOME=/opt/local/share/logtalk
		echo "... using Logtalk installation found at /opt/local/share/logtalk"
	elif [ -d "/opt/share/logtalk" ]; then
		LOGTALKHOME=/opt/share/logtalk
		echo "... using Logtalk installation found at /opt/share/logtalk"
	else
		echo "... unable to locate Logtalk installation directory!"
		echo
		exit 1
	fi
	echo
	export LOGTALKHOME=$LOGTALKHOME
elif ! [ -d "$LOGTALKHOME" ]; then
	echo "The environment variable LOGTALKHOME points to a non-existing directory!"
	echo "Its current value is: $LOGTALKHOME"
	echo "The variable must be set to your Logtalk installation directory!"
	echo
	exit 1
fi


prefix=`dirname "$LOGTALKHOME"`


while getopts "vlsh" Option
do
	case $Option in
		v) print_version;;
		l) list_versions;;
		s) show_selected;;
		h) usage_help;;
		*) usage_help;;
	esac
done


if [ "$1" == "" ]; then
	usage_help
else
	switch_version $1
	error=$?
	if [ 0 != $error ]; then
		echo "An error occurred when activating version \"$version\"!"
		exit 1
	fi
	exit 0
fi
