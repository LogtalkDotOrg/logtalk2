#/bin/bash

## ================================================================
## Logtalk - Open source object-oriented logic programming language
## Release 2.31.4
##
## Copyright (c) 1998-2008 Paulo Moura.  All Rights Reserved.
## ================================================================

source $LOGTALKHOME/integration/checks.sh

exec "$PLC"/plc -h 4096k -l 2048k -g 4096k -e "(consult('$LOGTALKHOME/integration/logtalk_plc.pl'), '\$root')." "$@"
