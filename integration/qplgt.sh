#/bin/bash

## ================================================================
## Logtalk - Open source object-oriented logic programming language
## Release 2.31.4
##
## Copyright (c) 1998-2008 Paulo Moura.  All Rights Reserved.
## ================================================================

source $LOGTALKHOME/integration/checks.sh

exec qp -s 3072 -d 1024 -h 2048 -g "['$LOGTALKHOME/integration/logtalk_qp.pl']." "$@"
