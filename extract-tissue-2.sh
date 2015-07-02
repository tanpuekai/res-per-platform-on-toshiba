#!/bin/bash

#files=`find . -name "GSE*_series_matrix.txt.gz"`

files=($(find . -name "GSE*_series_matrix.txt.gz"));

len1=${#files[@]};
echo $len1




for i in $(seq 1 $len1)
do
        let j=i-1

        fni=${files[$j]}
	if [ -f tmp.txt ]; then rm tmp*.txt; fi

	echo 	$fni
	zcat $fni| grep "^\"ID_REF" > tmp.txt
	echo $i

	zcat $fni|grep "^!Sample_title" >> tmp.txt
	zcat $fni|grep "^!Sample_source_name_ch1" >> tmp.txt
	zcat $fni|grep "^!Sample_characteristics_ch1"|grep -i tissue >> tmp.txt
	zcat $fni|grep "^!Sample_characteristics_ch2"|grep -i tissue >> tmp.txt

	cut -f2- tmp.txt > tmp.2.txt
	sed -i 's/""//g' tmp.2.txt
	sed -i 's/\t//g' tmp.2.txt
	sed -i 's/""/"\t"/g' tmp.2.txt

	awk -F'\t' '
	{ 
	    	for (i=1; i<=NF; i++)  {
        		a[NR,i] = $i
    		}
	}
	NF>p { p = NF }
	END {    
    		for(j=1; j<=p; j++) {
        		str=a[1,j]
        		for(i=2; i<=NR; i++){
            			str=str"\t"a[i,j];
        		}
        	print str
    		}
	}' tmp.2.txt > tmp.txt 

	cnt=`cat tmp.txt|wc -l`
	awk -F'\t' -v ith="$i" -v fn="$fni" -v tot="$cnt" '{print ith, "\t", tot, "\t", fn, "\t", $0}' tmp.txt> tmp-i.txt

############################################
	if [ $i -eq 1 ]; then
		mv tmp-i.txt per-individual-annot.txt
	else
		cat tmp-i.txt >> per-individual-annot.txt
	fi

	echo $i ":" $cnt $'\t' $fni

#	zcat $fni|grep "Sample_source_name_ch1" |tr -s '\t'  '\n'> tmp.txt
done


cut -f1-4 per-individual-annot.txt > first.4.txt
cut -f5- per-individual-annot.txt > five.txt
sed -i 's/\t/ /g' five.txt
sed -i 's/"//g' five.txt
paste first.4.txt five.txt > new.annot.txt


echo "finished"


