#!/bin/bash

#files=`find . -name "GSE*_series_matrix.txt.gz"`

files=($(find . -name "G*_series_matrix.txt.gz"));

len1=${#files[@]};
echo $len1


for i in $(seq 1 $len1)
do
        let j=i-1

        fni=${files[$j]}
	IDREF=`zcat $fni|grep "ID_REF" |cut -f2-|tr $'\t'  "@"`
	title=`zcat $fni|grep "Sample_title" |cut -f2-|tr $'\t'  "@"`
	source=`zcat $fni|grep "Sample_source_name_ch1" |cut -f2-|tr $'\t'  "@"`
	tissue1=`zcat $fni|grep "Sample_characteristics_ch1"|grep -i tissue|cut -f2-|tr $'\t'  "@"`
	tissue2=`zcat $fni|grep "Sample_characteristics_ch2"|grep -i tissue|cut -f2-|tr $'\t'  "@"`
        echo $fni $'\t' $IDREF $'\t' $title $'\t' $source  $'\t' $tissue1  $'\t'  $tissue2
#	zcat $fni|grep "Sample_source_name_ch1" |tr -s '\t'  '\n'> tmp.txt
done

