echo "Here we gooo with the Essen!"
echo "Starting with bi-grams...."

mint -d *.krn | rid -GLId | grep -v "[=r]" | grep -v "\[" > bi-gram-essen.tsv
mv bi-gram-essen.tsv ../../../corpus/ngrams/bi-gram-essen.tsv
