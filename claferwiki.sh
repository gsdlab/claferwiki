echo "-----------------------------------------"
echo "| ClaferWiki v0.5.1                     |"
echo "| https://github.com/gsdlab/claferwiki/ |"
echo "| By Michal Antkiewicz, Chris Walker    |"
echo "| Generative Software Development Lab   |"
echo "| Using `clafer -V`                    |"
echo "-----------------------------------------"
echo ""
echo "Starting gitit..."
echo ""

stack exec gitit -- -f gitit.cnf
