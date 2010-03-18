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
mkdir -p "$base/unit_test_results"
results="$base/unit_test_results"
rm -f "$results/*.results"
rm -f "$results/*.errors"

for unit in *
do
	if [ -d $unit ] ; then
		cd $unit
		if [ -e "./tester.lgt" ] ; then
			echo '*****************************************'
			echo "***** Testing $unit"
			echo '*****************************************'
			name=$(echo $unit|sed 's|/|_|g')
			swilgt -g "logtalk_load(tester),halt" > "$results/$name.results" 2> "$results/$name.errors"
		fi
		cd "$base"
	fi
done

echo '*****************************************'
echo "***** Error and warnings"
echo '*****************************************'
cd "$results"
grep -A1 'ERROR:' *.errors | tee errors.all
grep -A1 'WARNING!' *.results | tee -a errors.all
echo '*****************************************'
echo "***** Failed tests"
echo '*****************************************'
grep -A1 -i failure *.results | tee -a errors.all
