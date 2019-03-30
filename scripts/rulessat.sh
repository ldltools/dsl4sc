#! /bin/bash
# $Id: rulessat.sh,v 1.2 2018/02/08 13:02:55 sato Exp sato $
#
# (C) Copyright IBM Corp. 2018.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

set -Ceu -o pipefail

RULES2LDL=rules2ldl
LDLSAT=ldlsat
LDLSATOPTS=
VERSION=$($RULES2LDL --version)

usage ()
{
    echo "rulessat v$VERSION"
    echo "usage: `basename $0` [<infile>]"
    echo
    echo "`basename $0` is a SAT solver for dsl4sc"
    echo "`basename $0` reads a set of dsl4sc declarations from <infile>,"
    echo "converts them to a LDL formula φ, examines if φ is satisfiable or not, and then returns"
    echo "either \"valid\" (¬φ is unsatisfiable), \"satisfiable\", or \"unsatisfiable\""
    echo
    exit 0
}

infile=/dev/stdin
verbose=0

while test $# -gt 0
do
    case $1 in
	-h | --help)
	    usage
	    exit 0
	    ;;
	-V | --version)
	    echo $VERSION
	    exit 0
	    ;;
	-*)
	    LDLSATOPTS="$LDLSATOPTS $1"
	    ;;
	*) infile=$1
    esac
    shift
done

test -e "$infile" || { echo "$infile not found" > /dev/stderr; exit 1; } 

${RULES2LDL} $infile | $LDLSAT $LDLSATOPTS
