FILES="./games.lisp
./lisp101/number.lisp
./lisp101/reading.lisp 
./lisp101/any.lisp
./lisp101/caution.lisp
./lisp101/strings.lisp
./lisp101/array.lisp
./lisp101/list.lisp
./lisp101/debug.lisp
./lisp101/interact.lisp
./lisp101/random.lisp
./lisp101/hash.lisp
./lisp101/deftest.lisp
./game/tests.lisp
./game/stagger.lisp
./game/macros.lisp
./game/types.lisp
./game/printing.lisp
./game/defstructs.lisp
./game/roaming.lisp
./game/globals.lisp
./game/creation.lisp
./table/header.lisp
./table/structs.lisp
./table/table.lisp
./table/data.lisp
./table/xindex.lisp"

for i in $FILES; do
		echo "# $i"
		echo "#-------------------------------------------------------------"
		echo ""
		cat $i
		echo ""
done |cat -n > ~/tmp/code.txt

cd ~/tmp
a2ps --font-size 7 -g -o code.ps ~/tmp/code.txt
ps2pdf code.ps
open code.pdf
