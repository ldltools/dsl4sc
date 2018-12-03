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

usage () {
    echo "usage: `basename $0` <option>* <rules_file>"
    echo "options:"
    echo -e "\t-o <out_file>\toutput to <out_file>"
    echo -e "\t--until <stage>\tterminate when <stage> gets reached"
    echo -e "\t\t\t<stage> ::= rules | ldl | mso | dfa | dfadot | dfa[2-4] | scxml"
    echo -e "\t--monitor\tgenerate monitor"
    echo -e "\t--no-pp\t\tskip preprocessing (be careful)"
    echo -e "\t-v\t\tbecome verbose"
    echo -e "\t-h\t\tdisplay this message<outfile>"
}

RULESPP=rulespp
RULES2LDL=rules2ldl
LDL2MSO=ldl2mso
LDL2SCXML=ldl2scxml

infile=/dev/stdin
outfile=/dev/stdout
until=scxml
nopp=
verbose=0

while test $# -gt 0
do
    case $1 in
	-h | --help)
	    usage
	    exit 0
	    ;;
	-v | --verbose)
	    verbose=1
	    ;;
	--no-pp)
	    nopp="--no-pp"
	    shift
	    ;;
	-u | --until)
	    until=$2
	    shift
	    ;;
	-o | --output)
	    outfile=$2
	    shift
	    ;;
	--monitor)
	    # pass down to dfa2scxml.sh
	    export generate_monitor=1
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

if test $until = "rules"
then
    test $nopp = "--no-pp" && { cat $infile > $outfile; exit 0; }
    $RULESPP $infile -o $outfile
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
${RULES2LDL} $nopp $rulesfile -o $ldlfile --map $mapfile || { echo "** ${RULES2LDL} crashed" > /dev/stderr; rm -f $rulesfile $ldlfile $mapfile; exit 1; }

test $until = "ldl" && { ${LDL2MSO} $ldlfile --parse-only -t ldl > $outfile; rm -f $rulesfile $ldlfile $mapfile; exit 0; }

# --------------------------------------------------------------------------------
# rules -> rules in xml (which carries code fragments)
# --------------------------------------------------------------------------------
xmlrulesfile=$(tempfile -d /tmp/.dsl4sc -s .rules.xml)
test $verbose -eq 1 && echo "** ${RULESPP} : $rulesfile -> $xmlrulesfile" > /dev/stderr
${RULESPP} $rulesfile -o $xmlrulesfile -t xml || { echo "** ${RULESPP} crashed" > /dev/stderr; rm -f $rulesfile $xmlrulesfile; exit 1; }

rm -f $rulesfile

# --------------------------------------------------------------------------------
# ldl + map + rules in xml -> scxml
# --------------------------------------------------------------------------------
test $verbose -eq 1 && echo "** ${LDL2SCXML} : $ldlfile -> $mapfile -> $xmlrulesfile -> $outfile" > /dev/stderr

${LDL2SCXML} $ldlfile --map $mapfile --rules $xmlrulesfile --until $until -o $outfile || { echo "** ${LDL2SCXML} crashed"; rm -f $ldlfile $mapfile $xmlrulesfile; exit 1; }

rm -f $ldlfile $mapfile $xmlrulesfile
true
