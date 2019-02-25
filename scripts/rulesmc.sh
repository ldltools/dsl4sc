#! /bin/bash
# $Id: rulesmc.sh,v 1.2 2018/06/10 13:17:13 sato Exp sato $
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

RULESSAT=rulessat
RULESPP=rulespp
RULES2LDL=rules2ldl
LDLMC=ldlmc
VERSION=$($RULES2LDL --version)

usage ()
{
    echo "rulesmc v$VERSION"
    echo "usage: `basename $0` <option>* <model_file>? <infile>"
    echo
    echo "`basename $0` is a model-checker for dsl4sc"
    echo "`basename $0` reads a model M (from <model_file>) and a set of requirements φ (from <infile>),"
    echo "examines if M ⊨ φ (i.e., ⊨ M → φ) holds or not, and then returns"
    echo "either \"claim holds\" (M ⊨ φ holds) or \"claim does not hold\""
    echo
    exit 0
}


modelfile=
infile=/dev/stdin
infile_copied=0
reachability=0
verbose=0

ldlmcopts=
while test $# -gt 0
do
    case $1 in
	-m | --model)
	    modelfile=$2
	    shift ;;
	-r | --reachability)
	    reachability=1
	    ;;
	-h | --help)
	    usage
	    exit 0
	    ;;
	-v | --verbose)
	    verbose=1
	    ldlmcopts="$ldlmcopts -v"
	    ;;
	-V | --version)
	    echo $VERSION
	    exit 0
	    ;;
	-*)
	    ldlmcopts="$ldlmcopts $1"
	    ;;
	*)
	    test -z "$modelfile" && modelfile=$1 || infile=$1
    esac
    shift
done

test .$modelfile = . && { usage; exit 0; }
test -f $modelfile || { echo "$modelfile does not exit" > /dev/stderr; exit 1; }
test -e $infile || { echo "$infile does not exit" > /dev/stderr; exit 1; }
test ! -f $infile && { temp=`tempfile -s .rules`; cat $infile > $temp; infile=$temp; infile_copied=1; }

# --------------------------------------------------------------------------------
# find_missing (modelfile, infile)
# dump_missing (missing)
# --------------------------------------------------------------------------------
find_missing ()
{

local modelfile=$1    
local infile=$2
test -f $modelfile -a -e $infile || { echo "** error in detect_missing"; exit 1; } 

# modelfile, infile -> xmlfile1, xmlfile2
xmlfile1=`tempfile -s .xml`
xmlfile2=`tempfile -s .xml`
$RULESPP $modelfile -t xml -o ${xmlfile1} || exit 1
$RULESPP $infile -t xml -o ${xmlfile2} || exit 1
test -f ${xmlfile1} -a -f ${xmlfile2} || exit 1

# detect spurious events that appear in $infile but does not in $modelfile
cat <<EOF | xqilla /dev/stdin | tr '\n' ' ' |\
    { read rslt; test ."$rslt" = . || { echo "** strange events in \"$infile\"": $rslt > /dev/stderr; rm -f ${xmlfile1} ${xmlfile2}; exit 1; }; } || exit 1
declare default element namespace "https://github.com/ldltools/dsl4sc";
let \$events1 := doc("$xmlfile1")//events/event/@name
let \$events2 := doc("$xmlfile2")//events/event/@name
return
  for \$e2 in \$events2
  where empty (index-of (\$events1, \$e2))
  return data (\$e2)
EOF

# detect spurious propositions
cat <<EOF | xqilla /dev/stdin | tr '\n' ' ' |\
    { read rslt; test ."$rslt" = . || { echo "** strange propositions in \"$infile\"": $rslt > /dev/stderr; rm -f ${xmlfile1} ${xmlfile2}; exit 1; }; }
declare default element namespace "https://github.com/ldltools/dsl4sc";
let \$vars1 := doc("$xmlfile1")//propositions/proposition/@variable
let \$vars2 := doc("$xmlfile2")//propositions/proposition/@variable
return
  for \$p2 in \$vars2
  where empty (index-of (\$vars1, \$p2))
  return data (\$p2)
EOF

# find missing events
cat <<EOF | xqilla /dev/stdin | tr ' ' '\n'
declare default element namespace "https://github.com/ldltools/dsl4sc";
let \$events1 := doc("$xmlfile1")//events/event/@name
let \$events2 := doc("$xmlfile2")//events/event/@name
return
  for \$e1 in \$events1
  where empty (index-of (\$events2, \$e1))
  return data (\$e1)
EOF

# clean up
rm -f ${xmlfile1} ${xmlfile2}

}

dump_missing ()
{
  
local missing=$1
test -f $missing  || { echo "** error in dump_missing"; exit 1; } 

echo "event"

nline=`wc -l $missing | awk '{print ($1)}'`
test $verbose -eq 1 && { echo "** $nline missing events:" | cat - $missing > /dev/stderr; }

cat $missing |\
gawk -v N=$nline '{printf ("%s%s", $1, (NR == N) ? "" : ", ");}END{print(";")}'

echo

}

# --------------------------------------------------------------------------------
# mc (modelfile, infile)
# --------------------------------------------------------------------------------
mc ()
{

local modelfile=$1    
local infile=$2
test -f $modelfile -a -e $infile || { echo "** error in mc"; exit 1; } 

# infile -> propfile

missing=`tempfile` && touch $missing
find_missing $modelfile $infile > $missing

propfile=$infile
test -s $missing && { propfile=`tempfile -s .rules`; dump_missing $missing > $propfile; cat $infile >> $propfile; }

rm -f $missing

# ldl generation

ldlfile1=`tempfile -s .ldl`
ldlfile2=`tempfile -s .ldl`
${RULES2LDL} $modelfile -o ${ldlfile1} || exit 1
${RULES2LDL} $propfile -o ${ldlfile2} || exit 1

# clean up
test $propfile = $infile || rm -f $propfile

# ldlmc
$LDLMC $ldlmcopts --model ${ldlfile1} ${ldlfile2} || { echo "** rulesmc crashed" > /dev/stderr; exit 1; }

# clean up
rm -f ${ldlfile1} ${ldlfile2}

}

# --------------------------------------------------------------------------------
# ra (modelfile, infile)
# --------------------------------------------------------------------------------
ra ()
{
local modelfile=$1    
local infile=$2
test -f $modelfile -a -e $infile || { echo "** error in ra"; exit 1; } 

cat $modelfile $infile | $RULESSAT | { read result; if test "$result" = satisfiable; then echo reachable; else echo unreachable; fi; } || exit 1

}

# --------------------------------------------------------------------------------
# main
# --------------------------------------------------------------------------------

if test $reachability -eq 1
then ra $modelfile $infile
else mc $modelfile $infile
fi

test -f $infile -a ${infile_copied} -eq 1 && rm -f $infile
true
