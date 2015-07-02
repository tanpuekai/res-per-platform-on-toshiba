#!/bin/bash

cut -f2- per-individual-annot.txt > all.tis.words

sed -i 's/glands/gland/g' all.tis.words
sed -i 's/cells/cell/g' all.tis.words

sed -i 's/.progenitor./+progenitor+/g' all.tis.words


sed -i 's/.gland/+gland/g' all.tis.words
sed -i 's/.cell/+cell/g' all.tis.words
sed -i 's/"//g' all.tis.words
sed -i 's/-/ /g' all.tis.words
sed -i 's/_/ /g' all.tis.words
sed -i 's/\./ /g' all.tis.words
sed -i 's/,/ /g' all.tis.words
sed -i 's/:/ /g' all.tis.words
sed -i 's/\// /g' all.tis.words
sed -i 's/(\|)\|;/ /g' all.tis.words

cat all.tis.words |sed 's/ /\n/g'|sed 's/\t/\n/g'|sort|uniq -ic > freq.tis.words.txt

for j in {1..20}
do
	sed -i 's/^ //g' freq.tis.words.txt
done

sed -i 's/ /\t/g' freq.tis.words.txt

sort -nrk 1,1 freq.tis.words.txt > tmp.txt

mv tmp.txt freq.tis.words.txt

######################################

cat freq.tis.words.txt |grep "gland\|cell\|cyte\|blast\|theli"> top.cell.types


