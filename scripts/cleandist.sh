#!/bin/sh

## ================================================================
## Logtalk - Open source object-oriented logic programming language
## Release 2.32.1
##
## Copyright (c) 1998-2008 Paulo Moura. All Rights Reserved.
## 
## Logtalk is free software. You can redistribute it and/or modify
## it under the terms of the Artistic License 2.0 as published by 
## the The Perl Foundation.
## 
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
## Artistic License 2.0 for more details. A copy of the license is 
## provided in the "LICENSE.txt" file.
## ================================================================

find . -name .svn -print0 | xargs -0 rm -rf
find . -name CVS -print0 | xargs -0 rm -rf
find . -name .cvsignore -print0 | xargs -0 rm -f
find . -name '.#*' -print0 | xargs -0 rm -f
find . -name .DS_Store -print0 | xargs -0 rm -f
find . -name '.gdb*' -print0 | xargs -0 rm -f

find . -type f -print0 | xargs -0 chmod 644
find . -type d -print0 | xargs -0 chmod 755

chmod a+x integration/*.sh
chmod a+x manuals/userman/*.sh
chmod a+x manuals/refman/*.sh
chmod a+x scripts/*.sh
chmod a-x scripts/*.js
chmod a+x scripts/debian/postinst
chmod a+x scripts/debian/prerm
chmod a+x scripts/debian/postrm
chmod a+x scripts/linux/*.sh
chmod a+x scripts/macosx/postflight
chmod a+x xml/*.sh
chmod a-x xml/*.js
