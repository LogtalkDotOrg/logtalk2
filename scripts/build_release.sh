#!/bin/sh

## ================================================================
## Logtalk - Open source object-oriented logic programming language
## Release 2.38.2
## 
## Copyright (c) 1998-2010 Paulo Moura.        All Rights Reserved.
## Logtalk is free software.  You can redistribute it and/or modify
## it under the terms of the "Artistic License 2.0" as published by 
## The Perl Foundation. Consult the "LICENSE.txt" file for details.
## ================================================================

dir=`PWD`

svn export http://svn.logtalk.org/logtalk/trunk lgt2382

cd lgt2382
chmod a+x scripts/cleandist.sh
scripts/cleandist.sh

cd ..
cp -R lgt2382/manuals man2382
tar -czf man2382.tgz man2382
tar -cjf lgt2382.tar.bz2 lgt2382

mkdir -p debian/usr/bin
mkdir -p debian/usr/share/doc/logtalk
mkdir -p debian/usr/share/doc-base
mkdir -p debian/usr/share/menu
mkdir -p debian/DEBIAN
cd lgt2382/scripts
./install.sh $dir/debian/usr
rm -rf $dir/debian/usr/share/mime
cp debian/logtalk.doc-base $dir/debian/usr/share/doc-base/logtalk-docs
cp debian/menu $dir/debian/usr/share/menu/logtalk
cp ../*.bib $dir/debian/usr/share/doc/logtalk
cp ../*.txt $dir/debian/usr/share/doc/logtalk
cp debian/copyright $dir/debian/usr/share/doc/logtalk
cp debian/changelog $dir/debian/usr/share/doc/logtalk
cp debian/changelog.Debian $dir/debian/usr/share/doc/logtalk
gzip --best $dir/debian/usr/share/doc/logtalk/*.bib 
gzip --best $dir/debian/usr/share/doc/logtalk/*.txt 
gzip --best $dir/debian/usr/share/doc/logtalk/changelog 
gzip --best $dir/debian/usr/share/doc/logtalk/changelog.Debian
cp debian/control $dir/debian/DEBIAN
cp debian/postinst $dir/debian/DEBIAN
cp debian/prerm $dir/debian/DEBIAN
cp debian/postrm $dir/debian/DEBIAN
cd $dir
dpkg-deb --build debian logtalk_2.38.2-1_all.deb

md5="`md5 -q lgt2382.tar.bz2`"
sudo mkdir -p /opt/local/var/macports/distfiles/logtalk
sudo cp -f lgt2382.tar.bz2 /opt/local/var/macports/distfiles/logtalk/lgt2382.tar.bz2
cd /opt/local/var/macports/sources/rsync.macports.org/release/ports/lang/logtalk/
sudo mv -f Portfile Portfile.old
sudo cp $dir/lgt2382/scripts/macosx/Portfile .
sudo sed -e 's/^version.*/version 2.38.2/' -i '' Portfile
sudo sed -e "s/^checksums.*/checksums md5 $md5/" -i '' Portfile
sudo port clean --archive logtalk
sudo port destroot logtalk
sudo port pkg logtalk
cp -R work/logtalk-2.38.2.pkg $dir
sudo port clean logtalk

cd $dir
mkdir manpdf2382
cd man2382/userman
./userman.sh
mv userman.pdf ../../manpdf2382/lgtuserman2382.pdf
cd ../refman
./refman.sh
mv refman.pdf ../../manpdf2382/lgtrefman2382.pdf
cd ../..
tar -czf manpdf2382.tgz manpdf2382
