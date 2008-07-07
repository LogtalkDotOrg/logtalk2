#!/bin/sh

## ================================================================
## Logtalk - Open source object-oriented logic programming language
## Release 2.32.1
##
## Copyright (c) 1998-2008 Paulo Moura. All Rights Reserved.

Logtalk is free software. You can redistribute it and/or modify
it under the terms of the Artistic License 2.0 as published by 
the The Perl Foundation.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
Artistic License 2.0 for more details. A copy of the license is 
provided in the "LICENSE.txt" file.
## ================================================================

dir="$PWD"
cd ..

LOGTALKHOME=/usr/local/share/logtalk ./uninstall.sh
./install.sh

cd /usr/local/share
tar -cjf lgt2321.tar.bz2 lgt2321
mv lgt2321.tar.bz2 /usr/src/redhat/SOURCES

cd "$dir"
rpmbuild -ba --target=noarch-*-linux logtalk.spec

cd /usr/src/redhat/RPMS/noarch
echo $PWD
ls -l
