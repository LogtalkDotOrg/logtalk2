#!/bin/sh

## ================================================================
## Logtalk - Open source object-oriented logic programming language
## Release 2.31.5
##
## Copyright (c) 1998-2008 Paulo Moura.  All Rights Reserved.
## ================================================================

dir="$PWD"
cd ..

LOGTALKHOME=/usr/local/share/logtalk ./uninstall.sh
./install.sh

cd /usr/local/share
tar -cjf lgt2315.tar.bz2 lgt2315
mv lgt2315.tar.bz2 /usr/src/redhat/SOURCES

cd "$dir"
rpmbuild -ba --target=noarch-*-linux logtalk.spec

cd /usr/src/redhat/RPMS/noarch
echo $PWD
ls -l
