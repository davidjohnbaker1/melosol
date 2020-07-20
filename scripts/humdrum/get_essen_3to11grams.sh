echo "Here we gooo with the Essen!"
echo "All the grammmmsss"

# tri grams 
mint -d *.krn | rid -GLId | grep -v "[=r]" | grep -v "\[" | context -n 2 > tri-gram-essen.tsv
mv tri-gram-essen.tsv ../../../corpus/ngrams/tri-gram-essen.tsv

# quad grams 
mint -d *.krn | rid -GLId | grep -v "[=r]" | grep -v "\[" | context -n 3 > quad-gram-essen.tsv
mv quad-gram-essen.tsv ../../../corpus/ngrams/quad-gram-essen.tsv

# five grams 
mint -d *.krn | rid -GLId | grep -v "[=r]" | grep -v "\[" | context -n 4 > quint-gram-essen.tsv
mv quint-gram-essen.tsv ../../../corpus/ngrams/quint-gram-essen.tsv

# seven grams 
mint -d *.krn | rid -GLId | grep -v "[=r]" | grep -v "\[" | context -n 6 > hept-gram-essen.tsv
mv hept-gram-essen.tsv ../../../corpus/ngrams/hept-gram-essen.tsv

# nine grams 
mint -d *.krn | rid -GLId | grep -v "[=r]" | grep -v "\[" | context -n 8 > non-gram-essen.tsv
mv non-gram-essen.tsv ../../../corpus/ngrams/non-gram-essen.tsv

# eleven grams 
mint -d *.krn | rid -GLId | grep -v "[=r]" | grep -v "\[" | context -n 10 > hende-gram-essen.tsv
mv hende-gram-essen.tsv ../../../corpus/ngrams/hende-gram-essen.tsv


