#!/bin/sh

# echo "Setting environment for building SLAyer"


full=`pwd`/$BASH_SOURCE
SL_SRC="${full%/*}/../"
export SL_WIN=`cygpath -w $SL_SRC`
export SL_UNIX=`cygpath -u $SL_WIN`
#echo "Unix:  $SL_UNIX   Windows:  $SL_WIN"

# add dirs slayer builds are installed into to PATH
export PATH="$SL_UNIX/SLAyer/bin:$PATH"
export PATH="$SL_UNIX/SLAyer/tools/Z3/build:$PATH"

# add ocaml compilers to PATH
export PATH="$SL_UNIX/SLAyer/tools/ocaml/bin:$PATH"
export PATH="$SL_UNIX/SLAyer/tools/ocaml/lib/stublibs:$PATH"
export PATH="$SL_UNIX/SLAyer/tools/flexdll:$PATH"

# add ocaml library to INCLUDE, mainly for compiling C interface code
export INCLUDE="$SL_WIN\SLAyer\tools\ocaml\lib;$INCLUDE"

# must be in windows format
export OCAMLLIB="$SL_WIN\SLAyer\tools\ocaml\lib"
