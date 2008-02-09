#/bin/bash

## ================================================================
## Logtalk - Open source object-oriented logic programming language
## Release 2.31.4
##
## Copyright (c) 1998-2008 Paulo Moura.  All Rights Reserved.
## ================================================================

source $LOGTALKHOME/integration/checks.sh

exec yap -s 49152 -h 16384 -t 1024 -l "$LOGTALKHOME/integration/logtalk_yap.pl" "$@"
