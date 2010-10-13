#!/bin/sh

## ================================================================
## Logtalk - Open source object-oriented logic programming language
## Release 2.41.2
## 
## Copyright (c) 1998-2010 Paulo Moura.        All Rights Reserved.
## Logtalk is free software.  You can redistribute it and/or modify
## it under the terms of the "Artistic License 2.0" as published by 
## The Perl Foundation. Consult the "LICENSE.txt" file for details.
## ================================================================

dir=`PWD`

svn export http://svn.logtalk.org/logtalk/trunk lgt2412

cd lgt2412
chmod a+x scripts/cleandist.sh
scripts/cleandist.sh

cd ..
cp -R lgt2412/manuals man2412
tar -czf man2412.tgz man2412
tar -cjf lgt2412.tar.bz2 lgt2412

mkdir -p debian/usr/bin
mkdir -p debian/usr/share/doc/logtalk
mkdir -p debian/usr/share/doc-base
mkdir -p debian/usr/share/menu
mkdir -p debian/DEBIAN
cd lgt2412/scripts
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
dpkg-deb --build debian logtalk_2.41.2-1_all.deb

md5="`md5 -q lgt2412.tar.bz2`"
sha1="`openssl sha1 -r lgt2412.tar.bz2 | xargs -L 1 | sed 's/*lgt2412.tar.bz2//g'`"
rmd160="`openssl rmd160 -r lgt2412.tar.bz2 | xargs -L 1 | sed 's/*lgt2412.tar.bz2//g'`"
sudo mkdir -p /opt/local/var/macports/distfiles/logtalk
sudo cp -f lgt2412.tar.bz2 /opt/local/var/macports/distfiles/logtalk/lgt2412.tar.bz2
cd /opt/local/var/macports/sources/rsync.macports.org/release/ports/lang/logtalk/
sudo mv -f Portfile Portfile.old
sudo cp $dir/lgt2412/scripts/macosx/Portfile .
sudo sed -e 's/^version.*/version 2.41.2/' -i '' Portfile
sudo sed -e "s/md5.*/md5 $md5 \\\/" -i '' Portfile
sudo sed -e "s/sha1.*/sha1 $sha1 \\\/" -i '' Portfile
sudo sed -e "s/rmd160.*/rmd160 $rmd160/" -i '' Portfile
sudo port clean --archive logtalk
sudo port destroot logtalk
sudo port pkg logtalk
cp -R work/logtalk-2.41.2.pkg $dir
sudo port clean logtalk

cd $dir
mkdir manpdf2412
cd man2412/userman
./userman.sh
mv userman.pdf ../../manpdf2412/lgtuserman2412.pdf
cd ../refman
./refman.sh
mv refman.pdf ../../manpdf2412/lgtrefman2412.pdf
cd ../..
tar -czf manpdf2412.tgz manpdf2412
