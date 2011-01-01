#!/bin/sh

## ================================================================
## Logtalk - Open source object-oriented logic programming language
## Release 2.42.2
## 
## Copyright (c) 1998-2011 Paulo Moura.        All Rights Reserved.
## Logtalk is free software.  You can redistribute it and/or modify
## it under the terms of the "Artistic License 2.0" as published by 
## The Perl Foundation. Consult the "LICENSE.txt" file for details.
## ================================================================

dir="$PWD"

cd ../..
tar -cjf ~/rpmbuild/SOURCES/lgt2422.tar.bz2 .
mkdir -p ~/rpmbuild/RPMS/noarch

cd "$dir"
rpmbuild -ba --target=noarch-*-linux logtalk.spec

cd ~/rpmbuild/RPMS/noarch
echo $PWD
ls -l