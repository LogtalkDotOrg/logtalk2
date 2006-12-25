#!/bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.29.1
##
## Copyright (c) 1998-2006 Paulo Moura.  All Rights Reserved.
## =================================================================

cd ..
dir="$PWD"

port uninstall logtalk

LOGTALKHOME=/opt/local/logtalk ./lgt_uninstall.sh
./lgt_install.sh /opt/local

cd /opt/local/logtalk
scripts/cleandist.sh
chmod a+x scripts/*.sh
chmod a-x scripts/*.js
chmod a+x xml/*.sh
chmod a-x xml/*.js

cd ..
tar -czf lgt2291.tgz lgt2291
md5="`md5 -q lgt2291.tgz`"
mv lgt2291.tgz /opt/local/var/db/dports/distfiles/logtalk/lgt2291.tgz

cd "$dir"
LOGTALKHOME=/opt/local/logtalk ./lgt_uninstall.sh

cd /opt/local/var/db/dports/sources/rsync.rsync.darwinports.org_dpupdate_dports/lang/logtalk/
cp Portfile Portfile.old
sed -e 's/\(^version[[:space:]]*\).*/\1 2.29.1/' -i '.bak' Portfile
sed -e "s/\(^checksums[[:space:]]*md5[[:space:]]*\)\([[:alnum:]]\{32\}\)/\1 $md5/" -i '.bak' Portfile
sed -e 's/\(^distname[[:space:]]*\).*/\1 lgt2291/' -i '.bak' Portfile

port install logtalk
port pkg logtalk
