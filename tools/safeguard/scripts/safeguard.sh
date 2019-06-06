#! /bin/bash
#
# (C) Copyright IBM Corp. 2018.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# http://www.apache.org/licenses/LICENSE-2.0
#

set -eu

LIBDIR=$(readlink -f `dirname $0`/../lib/dsl4sc)

# helpers
RULES2SCXML=rules2scxml
GUARDGEN=${LIBDIR}/tools/guardgen.sh

usage ()
{
    echo "$(basename $0) -- attach pre/post conditions to event handlers"
    echo "usage: $(basename $0) <option>* <infile>"
    echo "options:"
    echo -e "  --spec <file>\t\tgenerate guards from spec defined in <file>"
    echo -e "  -g <file>\t\tread guards from <file> (exclusive with \"--spec\")"
    echo -e "  -o <outfile>\t\toutput to <outfile>"
    echo -e "  -v\t\t\tbecome verbose"
    echo -e "  -h\t\t\tdisplay this message"
}

abort ()
{
    local message=$1
    echo "** $message" > /dev/stderr
    exit 1
}

specfile=
spectype=

infile=/dev/stdin
intype=ts

outfile=/dev/stdout
verbose=0

while test $# -gt 0
do
    case $1 in
	--spec*)
	    specfile=$2
	    shift
	    ;;
	-g | --guard*)
	    specfile=$2
	    spectype=guards
	    shift
	    ;;

	-s)
	    intype=$2
	    shift
	    ;;
	-o | --output)
	    outfile=$2
	    shift
	    ;;

	-h | --help)
	    usage
	    exit 0
	    ;;
	-v | --verbose)
	    verbose=1
	    ;;

	-*)
	    abort "unknow option: $1"
	    ;;
	*) infile=$1
    esac
    shift
done

test -e "$infile" || abort "source file not found"

if test ! -e "$specfile"
then
    if test -f $infile
    then
	stem=${infile%.*}
	if test -f ${stem}.guards; then
	    specfile=${stem}.guards
	    spectype=guards
	else
	    if test -f ${stem}.dsl; then
		specfile=${stem}.dsl
		spectype=dsl
	    fi
	fi
    fi
fi
test -e "$specfile" || abort "spec not found"

# spec -> guards
guardgen ()
{
    test "$spectype" = guards && cat $specfile && return

    test -x $GUARDGEN || abort "$GUARDGEN not found"
    if test ."$spectype" != .
    then
	$GUARDGEN $specfile -s $spectype
    else
	$GUARDGEN $specfile
    fi
}

guardfile=$(tempfile -d /tmp/.dsl4sc -s .guards)
guardgen > $guardfile
cat $guardfile

rm -f $guardfile
