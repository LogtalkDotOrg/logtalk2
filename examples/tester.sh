#!/bin/sh

## ================================================================
## Logtalk - Open source object-oriented logic programming language
## Release 2.39.1
## 
## Copyright (c) 1998-2010 Paulo Moura.        All Rights Reserved.
## Logtalk is free software.  You can redistribute it and/or modify
## it under the terms of the "Artistic License 2.0" as published by 
## The Perl Foundation. Consult the "LICENSE.txt" file for details.
## ================================================================

# based on a unit test automation script contributed by Parker Jones


base="$PWD"
results="$base/tester_results"
backend=yap
prolog='YAP'
logtalk='yaplgt -g'

usage_help()
{
	echo 
	echo "This script automates running unit tests found on the sub-directories"
	echo "of the directory containing this script."
	echo
	echo "Usage:"
	echo "  $0 -p prolog -d results"
	echo "  $0 -h"
	echo
	echo "Optional arguments:"
	echo "  -p back-end Prolog compiler (default is $backend)"
	echo "  -d name of the sub-directory to store the test results (default is tester_results)"
	echo "  -h help"
	echo
	exit 1
}

while getopts "p:d:h" option
do
	case $option in
		p) p_arg="$OPTARG";;
		d) d_arg="$OPTARG";;
		h) usage_help;;
		*) usage_help;;
	esac
done

#if [ "$p_arg" = "b" ] ; then
#	prolog='B-Prolog'
#	logtalk='bplgt -g'
if [ "$p_arg" = "ciao" ] ; then
	prolog='Ciao Prolog'
	logtalk='ciaolgt -e'
elif [ "$p_arg" = "cx" ] ; then
	prolog='CxProlog'
	logtalk='cxlgt --goal'
elif [ "$p_arg" = "eclipse" ] ; then
	prolog='ECLiPSe'
	logtalk='eclipselgt -e'
#elif [ "$p_arg" = "qu" ] ; then
#	prolog='Qu-Prolog'
#	logtalk='qplgt -g'
elif [ "$p_arg" = "sicstus" ] ; then
	prolog='SICStus Prolog'
	logtalk='sicstuslgt --goal'
elif [ "$p_arg" = "swi" ] ; then
	prolog='SWI-Prolog'
	logtalk='swilgt -g'
elif [ "$p_arg" = "xsb" ] ; then
	prolog='XSB'
	logtalk='xsblgt -e'
elif [ "$p_arg" = "yap" ] ; then
	prolog='YAP'
	logtalk='yaplgt -g'
elif [ "$p_arg" != "" ] ; then
	echo "Error! Unsupported back-end Prolog compiler: $p_arg"
	usage_help
	exit 1
fi

if [ "$d_arg" != "" ] ; then
	results="$base/$d_arg"
fi

mkdir -p "$results"
rm -f "$results"/*.results
rm -f "$results"/*.errors
rm -f "$results"/errors.all

echo '********************************************'
echo "***** Running unit tests with $prolog"
for unit in *
do
	if [ -d $unit ] ; then
		cd $unit
		if [ -e "./tester.lgt" ] ; then
			echo '********************************************'
			echo "***** Testing $unit"
			name=$(echo $unit|sed 's|/|_|g')
			$logtalk "logtalk_load(tester),halt." > "$results/$name.results" 2> "$results/$name.errors"
		fi
		for subunit in *
		do
			if [ -d $subunit ] ; then
				cd $subunit
				if [ -e "./tester.lgt" ] ; then
					echo '********************************************'
					echo "***** Testing $unit/$subunit"
					subname=$(echo $unit/$subunit|sed 's|/|_|g')
					$logtalk "logtalk_load(tester),halt." > "$results/$subname.results" 2> "$results/$subname.errors"
				fi
				cd ..
			fi
		done
		cd "$base"
	fi
done

echo '********************************************'
echo "***** Errors and warnings"
echo '********************************************'
cd "$results"
grep -A1 'ERROR:' *.errors | tee errors.all
grep -A1 'WARNING!' *.results | tee -a errors.all
echo '********************************************'
echo "***** Failed tests"
echo '********************************************'
grep -A1 -i ': failure' *.results | tee -a errors.all
echo '********************************************'
