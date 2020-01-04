#!/bin/sh
#
# input -- pdf file-name 

gs -q -dNOCACHE -dNOPAUSE -dBATCH -dSAFER -sDEVICE=eps2write -dLanguageLevel=3 -sOutputFile=$1-%d.eps $1.pdf

# cropping EPS
files=$1-*.eps
for f in $files
do
    ps2epsi $f tmp.eps
    mv tmp.eps $f
done
