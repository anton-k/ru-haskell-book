#!/bin/bash
set -e # abort script when a command exits with non-zero status

cd ~/haskell-notes/tex

xelatex --shell-escape notes-all.tex
#rm *.aux 
evince notes-all.pdf&

