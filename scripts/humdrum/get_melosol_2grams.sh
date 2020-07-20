# This Script Creates the MeloSol n-grams 

echo "Here we gooo!"
echo "Starting with bi-grams...."

mint -d *.krn | rid -GLId | grep -v "[=r]" | grep -v "\[" > bi-gram.tsv
mv bi-gram.tsv ../ngrams/bi-gram.tsv
