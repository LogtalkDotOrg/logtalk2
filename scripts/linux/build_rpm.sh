#!/bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.30.0
##
## Copyright (c) 1998-2007 Paulo Moura.  All Rights Reserved.
## =================================================================

dir="$PWD"
cd ..

LOGTALKHOME=/usr/local/share/logtalk ./lgt_uninstall.sh
./lgt_install.sh

cd /usr/local/share
tar -czf lgt2300.tgz lgt2300
mv lgt2300.tgz /usr/src/redhat/SOURCES

cd "$dir"
rpmbuild -ba --target=noarch-*-linux logtalk.spec

cd /usr/src/redhat/RPMS/noarch
echo $PWD
ls -l
