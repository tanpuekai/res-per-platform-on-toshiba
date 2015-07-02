#!/bin/bash

cat per-individual-annot.txt|grep -v  GSE50005_series_matrix.txt.gz > per-individual-annot-purified.txt

sed  -i 's/ .\/set/\t .\/set/g' per-individual-annot-purified.txt

sed  -i 's/series_matrix.txt.gz /series_matrix.txt.gz\t/g' per-individual-annot-purified.txt


