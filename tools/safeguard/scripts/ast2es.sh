#! /bin/bash
#
# JS/AltJS code generators (for transpilation)
# - babel generator (v7)
#   https://github.com/babel/babel/tree/master/packages/babel-generator
#   packages: @babel/core @babel/generator @babel/preset-typescript
# - escodegen
#   https://github.com/estools/escodegen
#   packages: escodegen
#

CODEGEN=${CODEGEN:-escodegen}

infile=/dev/stdin
outfile=/dev/stdout

escodegen ()
{
local tempfile=$(tempfile -s .js)
cat $infile > $tempfile
cat <<EOF | node > $outfile || rm -f $tempfile
const fs = require ('fs');
const escodegen = require ('escodegen')
fs.readFile ('$tempfile', 'utf8', function (err, str) {
    if (err) throw err;
    //console.log (str);
    var ast = JSON.parse (str);
    //escodegen.attachComments(ast, ast.comments);
    const opts = {comment: true, format: { preserveBlankLines: true }}
    console.log (escodegen.generate (ast, opts))
});
EOF
rm -f $tempfile
}

usage () {
    echo "ast2es"
    echo "usage: `basename $0` <option>* <ast_file>"
}

verbose=0

while test $# -gt 0
do
    case $1 in
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
	    echo "** unknow option: $1"
	    exit 1
	    ;;
	*) infile=$1
    esac
    shift
done

test -e "$infile" || { echo "$infile not found" > /dev/stderr; exit 1; }

case $CODEGEN in
    escodegen)
	escodegen
	;;
    *)
	echo "invalid specification: \"$CODEGEN\""
	exit 1
esac
