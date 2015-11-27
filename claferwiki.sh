echo "-----------------------------------------"
echo "| ClaferWiki v0.4.2.1                   |"
echo "| https://github.com/gsdlab/claferwiki/ |"
echo "| By Michal Antkiewicz, Chris Walker    |"
echo "| Generative Software Development Lab   |"
echo "| Using `clafer -V`                   |"
echo "-----------------------------------------"
echo ""
echo "Starting gitit..."
echo ""

if [ "$1" == "--sandbox" ]; then
	if [ -z "$2" ]; then
		cabal sandbox init --sandbox=../.clafertools-cabal-sandbox
	else
		cabal sandbox init --sandbox=$2
	fi
	cabal exec gitit -- -f gitit.cnf
else
	gitit -f gitit.cnf
fi
