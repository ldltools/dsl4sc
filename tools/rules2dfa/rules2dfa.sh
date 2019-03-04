#! /bin/bash
# $Id: rules2dfa.sh,v 1.1 2018/02/09 10:19:00 sato Exp sato $
#
# (C) Copyright IBM Corp. 2019.
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

BINDIR=$(readlink -f `dirname $0`)
LIBDIR=$BINDIR/rules2dfa_helpers

RULESPP=rulespp
RULES2LDL=rules2ldl
LDL2MSO=ldl2mso
LDL2DFA=$BINDIR/ldl2dfa.sh
VERSION=$($RULES2LDL --version)

usage () {
    echo "rule2dfa v$VERSION"
    echo "usage: `basename $0` <option>* <rules_file>"
    echo "options:"
    echo -e "  -o <out_file>\t\toutput to <out_file>"
    echo -e "  -p, --parse-only\tparse-only"
    echo -e "  -E, --rulespp-only\tpreprocess-only"
    echo -e "  --until <stage>\tterminate when <stage> gets reached"
    echo -e "  \t\t\t<stage> ::= spec | ldl | mso | dfa[1-2] | dfadot | dfa"
    echo -e "  --monitor\t\tgenerate monitor"
    echo -e "  -v\t\t\tbecome verbose"
    echo -e "  -h\t\t\tdisplay this message<outfile>"
}

infile=/dev/stdin
outfile=/dev/stdout
verbose=0

until=dfa
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
    test ${opt_skip_rulespp} -eq 1 && { cat $infile > $outfile; exit 0; }
    $RULESPP $infile -o $outfile
    exit 0
fi

if test $until = spec
then
    ${RULES2LDL} --until spec $infile -o $outfile
    exit 0
fi

mkdir -p /tmp/.dsl4sc
rulesfile=$(tempfile -d /tmp/.dsl4sc -s .rules)
cat $infile > $rulesfile

# --------------------------------------------------------------------------------
# rules -> ldl + map (b.w. event names and ldl propositions)
# --------------------------------------------------------------------------------
ldlfile=$(tempfile -d /tmp/.dsl4sc -s .ldl)
mapfile=$(tempfile -d /tmp/.dsl4sc -s .map)
test $verbose -eq 1 && echo "** ${RULES2LDL} : $rulesfile -> (${ldlfile}, ${mapfile})" > /dev/stderr
${RULES2LDL} $rulesfile -o $ldlfile --map $mapfile || { echo "** ${RULES2LDL} crashed" > /dev/stderr; rm -f $rulesfile $ldlfile $mapfile; exit 1; }

test $until = "ldl" && { ${LDL2MSO} $ldlfile --parse-only -t ldl > $outfile; rm -f $rulesfile $ldlfile $mapfile; exit 0; }

# --------------------------------------------------------------------------------
# ldl -> dfa1
# --------------------------------------------------------------------------------
dfa1file=$(tempfile -d /tmp/.dsl4sc -s .dfa1)
#echo "ldl2scxml.ldl2dfa : $infile -> ${dfa1file}" > /dev/stderr
test -x ${LDL2DFA} || { echo "** ${LDL2DFA} not found"; exit 1; }
${LDL2DFA} $ldlfile -o ${dfa1file} -u $until || { echo "** ${LDL2DFA} crashed" > /dev/stderr; rm -f ${dfa1file}; exit 1; }

case $until in
    ldl | mso)
	cat ${dfa1file} > $outfile; rm -f ${dfa1file}; exit 0 ;;
    dfa1)
	xmllint --format ${dfa1file} > $outfile; rm -f ${dfa1file}; exit 0 ;;
    dfadot)
	cat ${dfa1file} > $outfile; rm -f ${dfa1file}; exit 0 ;;
esac

rm -f $ldlfile

# --------------------------------------------------------------------------------
# rules -> rules in xml (which carries code fragments)
# --------------------------------------------------------------------------------
xmlrulesfile=$(tempfile -d /tmp/.dsl4sc -s .rules.xml)
test $verbose -eq 1 && echo "** ${RULESPP} : $rulesfile -> $xmlrulesfile" > /dev/stderr
${RULESPP} $rulesfile -o $xmlrulesfile -t xml || { echo "** ${RULESPP} crashed" > /dev/stderr; rm -f $rulesfile $xmlrulesfile; exit 1; }

rm -f $rulesfile

# --------------------------------------------------------------------------------
# dfa1 + map + rules in xml -> dfa2
# (dfa2 combines dfa, map (event->bits), and rules that carry code)
# --------------------------------------------------------------------------------

decode_events=$LIBDIR/decode_events.xq
include_rules=$LIBDIR/include_rules.xq
test -f ${decode_events} || { echo "${decode_events} not found" > /dev/stderr; exit 1; }
test -f ${include_rules} || { echo "${include_rules} not found" > /dev/stderr; exit 1; }

main1="local:decode_events (.)"
main2="local:include_rules (.)"
main3="local:include_rules (local:decode_events (.))"

case $until in
    dfa2-1) main=${main1} ;;
    dfa2-2) main=${main2} ;;
    *) main=${main3} ;;
esac

test -f "$mapfile" || { echo "** spurious map ($mapfile)" > /dev/stderr; exit 1; }
test -f "$xmlrulesfile" || { echo "** spurious rules ($xmlrulesfile)" > /dev/stderr; exit 1; }

dfa2file=$(tempfile -d /tmp/.dsl4sc -s .dfa2)
#echo "preprocess : $infile -> ${dfa2file}"
cat <<EOF | xqilla /dev/stdin -i ${dfa1file} -o ${dfa2file} || { echo "** xqilla crashed" > /dev/stderr; rm -f ${dfa2file}; exit 1; }
declare default element namespace "https://github.com/ldltools/dsl4sc";
declare variable \$alist := doc("`readlink -f $mapfile`")//bits;
`cat ${decode_events}`
declare variable \$rules := doc("`readlink -f $xmlrulesfile`")//rules/rule;
declare variable \$vars := doc("`readlink -f $xmlrulesfile`")//variables/variable;
declare variable \$scripts := doc("`readlink -f $xmlrulesfile`")//scripts/script;
`cat ${include_rules}`
$main
EOF

rm -f ${dfa1file} $mapfile $xmlrulesfile

case $until in
    dfa | dfa2*)
	xmllint --format ${dfa2file} > $outfile
	;;
    *)
	cat ${dfa2file} > $outfile
esac	

rm -f ${dfa2file}
true
