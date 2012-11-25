#!/bin/bash
# set -e # abort script when a command exits with non-zero status

# important flag "-8bit" to cure tab displaying bug in xelatex
xelatex --shell-escape notes.tex
#rm *.aux
evince notes.pdf&

