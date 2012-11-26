#!/bin/bash
# set -e # abort script when a command exits with non-zero status
dir=target
pandoc -sS --toc \
    --epub-stylesheet=epub-style.css \
    --epub-embed-font=liberation_serif/LiberationSerif-Italic.ttf \
    --epub-embed-font=liberation_serif/LiberationSerif-Regular.ttf \
    --epub-embed-font=liberation_serif/LiberationSerif-BoldItalic.ttf \
    --epub-embed-font=liberation_serif/LiberationSerif-Bold.ttf \
    --epub-metadata=metadata.xml \
    -o ../target/epub/ru-haskell-book.epub \
        title.txt \
        ../$dir/preface.txt \
        ../$dir/1.txt \
        ../$dir/2.txt \
        ../$dir/3.txt \
        ../$dir/4.txt \
        ../$dir/5.txt \
        ../$dir/6.txt \
        ../$dir/7.txt \
        ../$dir/8.txt \
        ../$dir/9.txt \
        ../$dir/10.txt \
        ../$dir/11.txt \
        ../$dir/12.txt \
        ../$dir/13.txt \
        ../$dir/14.txt \
        ../$dir/15.txt \
        ../$dir/16.txt \
        ../$dir/17.txt \
        ../$dir/18.txt \
        ../$dir/19.txt \
        ../$dir/20.txt \
        ../$dir/21.txt \
        ../$dir/appendix.txt

