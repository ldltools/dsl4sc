#! /bin/bash
# $Id: mso2dfa.sh,v 1.1 2018/02/09 10:19:12 sato Exp sato $
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

BINDIR=$(readlink -f `dirname $0`)
LIBDIR=$BINDIR/mso2dfa_helpers

infile=/dev/stdin
outfile=/dev/stdout

while test $# -gt 0
do
    case $1 in
	-h | --help)
	    echo "usage: `basename $0` <mso_file>"
	    exit 0
	    ;;
	-o | --output)
	    outfile=$2
	    shift
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
#test -f ${outfile} && { echo \"${outfile}\" already exists; exit 1; }

mkdir -p /tmp/.dsl4sc

# --------------------------------------------------------------------------------
# mso -(mona)-> state machine description
# $infile -> $msooutfile
# --------------------------------------------------------------------------------
monaoutfile=$(tempfile -d /tmp/.dsl4sc -s .monaout)
sed '/^#/d' $infile | mona -w -q /dev/stdin > ${monaoutfile} || { rm -f ${monaoutfile}; echo "** mona crashed" > /dev/stderr; exit 1; }

# --------------------------------------------------------------------------------
# dfa0 (in xml) generation
# $monaoutfile -> $dfa0file
# --------------------------------------------------------------------------------
dfa0file=$(tempfile -d /tmp/.dsl4sc -s .dfa0)

#
print_props ()
{
gawk '
BEGIN {
  processing = 0; count = 0;
  print ("<variables>");
}
/^A satisfying example/ { processing = 1; next; }
/^$/ { if (processing) exit; }
{ if (!processing) next; }
{ printf ("<variable name=\"%s\" type=\"prop\" number=\"%d\"/>\n", $1, count++); }
END { print ("</variables>"); }'
}

print_states ()
{
gawk '
BEGIN {
  processing = 0; state_id = -1;
  print ("<states>");
  #print ("<state id=\"init\"/>");
}
/^Accepting states:/ {
  print ("<accepting>");
  i = 3; while (i <= NF) { printf ("<id>q%d</id>", $i); i++; }
  print ("\n</accepting>");
}
/^Rejecting states:/ {
  print ("<rejecting>");
  i = 3; while (i <= NF) { printf ("<id>q%d</id>", $i); i++; }
  print ("\n</rejecting>");
}
/^Transitions:/ { processing = 1; next; }
/^$/ { if (processing) exit; }
{ if (!processing) next; }
/^State / {
   i = gensub (/:/, "", "g", $2) + 0;
   if (i <= state_id) next;
   state_id = i;
   if (i == 0) next;	# discard state 0
   printf ("<state id=\"q%d\"", state_id);
   printf ((state_id == 1) ? " initial=\"true\"" : "");
   printf ("/>\n");
}
END { print ("</states>"); }'
}

print_transitions ()
{
gawk '
BEGIN {
  processing = 0;
  print ("<transitions>");
  #print ("<transition from=\"init\" to=\"0\"/>");
  #print ("<transition from=\"init\" to=\"1\"/>");
}
/^Transitions:/ { processing = 1; next; }
/^$/ { if (processing) exit; }
{ if (!processing) next; }
/^State / {
   i = gensub (/:/, "", "g", $2) + 0;
   if ($4 == "->") {
     j = $6 + 0; lbl = $3;
     #lbl = (NF == 6) ? $3 : "true";
   }
   else {
     j = $5 + 0; lbl = "";
   }
   if (i == 0 && j == 1) next;	# discard transition 0->1
   printf ("<transition from=\"q%d\" to=\"q%d\" label=\"%s\"/>\n", i, j, lbl);
}
END { print ("</transitions>"); }'
}

#
echo -n					>  ${dfa0file}
echo "<dfa xmlns=\"https://github.com/ldltools/dsl4sc\">"	>> ${dfa0file}

cat ${monaoutfile} | print_props	>> ${dfa0file}
cat ${monaoutfile} | print_states	>> ${dfa0file}
cat ${monaoutfile} | print_transitions	>> ${dfa0file}

echo "</dfa>"				>> ${dfa0file}

rm -f ${monaoutfile}

# --------------------------------------------------------------------------------
# post-processing -- replace "label=01X..." with "label=l1 l2 .."
# dfa0file -> $outfile
# --------------------------------------------------------------------------------
#
xq_script=$LIBDIR/adjust_labels.xq
test -f ${xq_script} || { echo "${xq_script} not found" > /dev/stderr; exit 1; }

cat <<EOF | xqilla /dev/stdin -i ${dfa0file} -o $outfile || { echo "** xqilla crashed" > /dev/stderr; rm -f ${dfa0file}; exit 1; }
declare default element namespace "https://github.com/ldltools/dsl4sc";
`cat ${xq_script}`
local:adjust_labels (.)
EOF

rm -f ${dfa0file}
true
