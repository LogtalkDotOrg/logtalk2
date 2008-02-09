#/bin/bash

## ================================================================
## Logtalk - Open source object-oriented logic programming language
## Release 2.31.4
##
## Copyright (c) 1998-2008 Paulo Moura.  All Rights Reserved.
## ================================================================

source $LOGTALKHOME/integration/checks.sh

exec prolog +l "$LOGTALKHOME/integration/logtalk_quintus.pl" +z "$LOGTALKHOME" "$LOGTALKUSER" "$@"
