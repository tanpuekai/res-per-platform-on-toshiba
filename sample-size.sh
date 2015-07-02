#!/bin/bash

for i in `find . -name "GSE*txt.gz"`
do
	cnt=`zless $i |grep "ID_REF"|tr '\t' '\n'|grep -v ID_REF|wc -l`
	echo $i $'\t' $cnt
done
