#!/bin/sh -v
# Remove entries that have 's ... they already exist
# For some reason sed chokes on the input
# sed "/'s/d" < ../bf_dict.txt > bf_dict.tmp
perl -pi -e "s/\\'s//" <  ../bf_dict.txt > bf_dict.tmp
# convert to lower case
# tr '[:upper:]' '[:lower:]' < tt1.tmp > ../bf_dict.tmp
/Library/Frameworks/R.framework/Resources/Rscript make_unique.R
rm bf_dict.tmp 
