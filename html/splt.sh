#!/bin/sh
#
# ввод -- имя pdf-файла без расширения

gs -q -dNOCACHE -dNOPAUSE -dBATCH -dSAFER -sDEVICE=eps2write -sOutputFile=$1-%d.eps $1.pdf
