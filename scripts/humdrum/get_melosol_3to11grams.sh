# This Script Creates the MeloSol n-grams 

echo "Here we gooo!"
echo "Starting with tri-grams...."

# three grams 
mint -d *.krn | rid -GLId | grep -v "[=r]" | grep -v "\[" | context -n 2 > tri-gram.tsv
mv tri-gram.tsv ../ngrams/melosol-tri-gram.tsv

echo "now four"

# four grams 
mint -d *.krn | rid -GLId | grep -v "[=r]" | grep -v "\[" | context -n 3 > four-gram.tsv
mv four-gram.tsv ../ngrams/melosol-four-gram.tsv

echo "now five"

# five grams 
mint -d *.krn | rid -GLId | grep -v "[=r]" | grep -v "\[" | context -n 4 > five-gram.tsv
mv five-gram.tsv ../ngrams/melosol-five-gram.tsv

echo "now seven"

# seven grams 
mint -d *.krn | rid -GLId | grep -v "[=r]" | grep -v "\[" | context -n 6 > seven-gram.tsv
mv seven-gram.tsv ../ngrams/melosol-seven-gram.tsv

echo "now nine"

# nine grams 
mint -d *.krn | rid -GLId | grep -v "[=r]" | grep -v "\[" | context -n 8 > nine-gram.tsv
mv nine-gram.tsv ../ngrams/melosol-nine-gram.tsv

echo "now eleven"

# eleven grams 
mint -d *.krn | rid -GLId | grep -v "[=r]" | grep -v "\[" | context -n 10 > eleven-gram.tsv
mv eleven-gram.tsv ../ngrams/melosol-eleven-gram.tsv

echo "done"
