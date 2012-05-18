cd ~/haskell-notes/tex

xelatex --shell-escape notes-all.tex
#rm *.aux 
evince notes-all.pdf&

