#! /bin/bash
# $Id: rules2scxml.sh,v 1.1 2018/02/09 10:19:00 sato Exp sato $
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
#

RULESPP=rulespp
RULES2LDL=rules2ldl
LDL2MSO=ldl2mso
BINDIR=$(readlink -f `dirname $0`/../lib/dsl4sc/tools)
RULES2DFA=$BINDIR/rules2dfa.sh
DFA2SCXML=$BINDIR/dfa2scxml.sh
VERSION=$($RULES2LDL --version)
test -x $RULES2DFA || { echo "$RULES2DFA not found"; exit 1; }
test -x $DFA2SCXML || { echo "$DFA2SCXML not found"; exit 1; }

usage () {
    echo "rule2scxml v$VERSION"
    echo "usage: `basename $0` <option>* <rules_file>"
    echo "options:"
    echo -e "  -o <out_file>\t\toutput to <out_file>"
    echo -e "  -p, --parse-only\tparse-only"
    echo -e "  -E, --rulespp-only\tpreprocess-only"
    echo -e "  --until <stage>\tterminate when <stage> gets reached"
    echo -e "  \t\t\t<stage> ::= spec | ldl | dfa | dfa[0-3] | scxml"
    echo -e "  --auto-exit\t\tset the final transitions to be event-less"
    echo -e "  --exit=<e>\t\tset <e> as the event name for the final transitions"
    echo -e "  --monitor\t\tgenerate monitor"
    echo -e "  -v\t\t\tbecome verbose"
    echo -e "  -h\t\t\tdisplay this message"
}

infile=/dev/stdin
outfile=/dev/stdout
verbose=0

until=scxml
opt_parse_only=0
opt_rulespp_only=0

while test $# -gt 0
do
    case $1 in
	-o | --output)
	    outfile=$2
	    shift
	    ;;

	-p | --parse-only)
	    opt_parse_only=1
	    ;;
	-E | --rulespp-only)
	    opt_rulespp_only=1
	    ;;
	-u | --until)
	    until=$2
	    shift
	    ;;

	## extra options (for dfa2scxml.sh)
	--auto-exit)
	    # pass down to dfa2scxml.sh
	    export accept_transition=" "
	    ;;
	--exit=*)
	    export accept_transition=${1##*=}
	    ;;
	--monitor)
	    # pass down to dfa2scxml.sh
	    export generate_monitor=1
	    ;;

	-h | --help)
	    usage
	    exit 0
	    ;;
	-v | --verbose)
	    verbose=1
	    ;;
	-V | --version)
	    echo $VERSION
	    exit 0
	    ;;

	-*)
	    echo "** unknow option: $1"
	    exit 1
	    ;;
	*) infile=$1
    esac
    shift
done

test -e ${infile} || { echo \"${infile}\" not found; exit 1; }

if test ${opt_parse_only} -eq 1
then
    $RULESPP --parse-only $infile -o $outfile
    exit 0
fi

if test ${opt_rulespp_only} -eq 1
then
    $RULESPP $infile -o $outfile
    exit 0
fi

case $until in
spec)
    ${RULES2LDL} --until spec $infile -o $outfile
    exit 0 ;;
ldl)
    ${RULES2LDL} $infile -o $outfile
    exit 0 ;;
mso)
    ${RULES2LDL} $infile | ${LDL2MSO} -o $outfile
    exit 0 ;;
esac

mkdir -p /tmp/.dsl4sc
rulesfile=$(tempfile -d /tmp/.dsl4sc -s .rules)
cat $infile > $rulesfile

# --------------------------------------------------------------------------------
# rules -> dfa
# --------------------------------------------------------------------------------

dfafile=$(tempfile -d /tmp/.dsl4sc -s .dfa)
$RULES2DFA --until $until $rulesfile -o $dfafile || { echo "** $RULES2DFA crashed" > /dev/stderr; rm -f $dfafile; exit 1; }

case $until in
dfa | dfa0 | dfa1 | dfadot)
    cat $dfafile > $outfile
    rm -f $rulesfile $dfafile
    exit 0 ;;
*)
esac

rm -f $rulesfile

# --------------------------------------------------------------------------------
# dfa -> scxml
# --------------------------------------------------------------------------------

$DFA2SCXML --until $until $dfafile -o $outfile || { echo "** $DFA2SCXML crashed" > /dev/stderr; rm -f $dfafile; exit 1; }

rm -f $dfafile
true
