#! /bin/bash
# $Id: dfa2dot.sh,v 1.1 2018/02/09 10:19:12 sato Exp sato $

infile=/dev/stdin
outfile=/dev/stdout
#dfa2dfa_opts=
#dfa2dfa_opts="--merge-transitions"

while test $# -gt 0
do
    case $1 in
	-o | --output)
	    outfile=$2
	    shift
	    ;;
	-h | --help)
	    echo "usage: `basename $0` <dfa_file>"
	    exit 0
	    ;;
#	--*)
#	    dfa2dfa_opts="${dfa2dfa_opts} $1"
#	    ;;
	*) infile=$1
    esac
    shift
done

test -e $infile || { echo "$infile not found" > /dev/stderr; exit 1; }
#echo "dfa2dot : $infile -> $outfile" > /dev/stderr

# [preprocessing]
#BINDIR=$(readlink -f `dirname $0`)
#DFA2DFA=$BINDIR/dfa2dfa.sh
#test -x ${DFA2DFA} || { echo "${DFA2DFA} not found" > /dev/stderr; exit 1; }
#mkdir -p /tmp/.dsl4sc
#tempfile=$(tempfile -d /tmp/.dsl4sc -s .dfa.xml)
#echo "dfa2dot.dfa2dfa : $infile -> $tempfile" > /dev/stderr
#${DFA2DFA} ${dfa2dfa_opts} $infile -o $tempfile || { echo "** dfa2dfa crashed" > /dev/stderr; rm -f $tempfile; exit 1; }

# [dot generation]
dotfile=$(tempfile -d /tmp/.dsl4sc -s .dot)
#echo "dfa2dot.dfa2dot : $tempfile -> $dotfile" > /dev/stderr
echo "digraph g {" > $dotfile
xmllint --format $infile |\
gawk '
BEGIN { nprop = 0 }
/<state / {
  # id
  id = gensub (/.*id=\"([^\"]*)\".*/, "\\1", "g", $$0)
  printf ("  %s", id)
  printf ("  [");

  # label
  label = ""
  if ($$0 ~ /label=/) label = gensub (/.*label=\"([^\"]*)\".*/, "\\1", "g", $$0)
  printf ("%s", (label != "") ? ("label=\"" label "\",") : "")

  # initial state
  #if (id == "init") printf ("shape=point,");
  if (id == "1") printf ("shape=box,");

  # acc/rej
  if ($$0 ~ /accepting=/) printf ("shape=doublecircle,style=filled,fillcolor=lightgrey,")
  if ($$0 ~ /rejecting=/) printf ("style=filled,fillcolor=red,")

  printf ("comment=\"\"");
  printf ("]\n");

}
/<transition / {
  n1 = gensub (/.*from=\"([^\"]*)\".*/, "\\1", "g", $$0)
  n2 = gensub (/.*to=\"([^\"]*)\".*/, "\\1", "g", $$0)
  label = ""
  if ($$0 ~ /label=/) label = gensub (/.*label=\"([^\"]*)\".*/, "\\1", "g", $$0)
  printf ("  %s -> %s%s\n", n1, n2, (label != "") ? (" [label=\"" label "\"]") : "")
}
/<proposition / {
  name = gensub (/.*variable=\"([^\"]*)\".*/, "\\1", "g", $$0);
  prop[nprop] = name;
  nprop++
}
END {
  if (nprop == 0) exit 1;
  printf ("label = \"")
  i = 0; while (i < nprop) { printf ("%d:%s\\n", i, prop[i]); i++; }
  printf ("\"\n")
  printf ("labelloc = t\n")
  printf ("labeljust = r\n")
}' >> $dotfile
echo "}" >> $dotfile

#echo "dfa2dot.cat : $dotfile -> $outfile" > /dev/stderr
cat $dotfile > $outfile
rm -f $dotfile
true
