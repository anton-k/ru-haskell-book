#!/bin/bash
# set -e # abort script when a command exits with non-zero status

pandoc -S --toc \
    --epub-stylesheet=epub-style.css \
    --epub-embed-font=liberation_serif/LiberationSerif-Italic.ttf \
    --epub-embed-font=liberation_serif/LiberationSerif-Regular.ttf \
    --epub-embed-font=liberation_serif/LiberationSerif-BoldItalic.ttf \
    --epub-embed-font=liberation_serif/LiberationSerif-Bold.ttf \
    --epub-metadata=metadata.xml \
    -o ru-haskell-book.epub \
        title.txt \
        ../pandoc/preface.txt \
        ../pandoc/1.txt \
        ../pandoc/2.txt \
        ../pandoc/3.txt \
        ../pandoc/4.txt \
        ../pandoc/5.txt \
        ../pandoc/6.txt \
        ../pandoc/7.txt \
        ../pandoc/8.txt \
        ../pandoc/9.txt \
        ../pandoc/10.txt \
        ../pandoc/11.txt \
        ../pandoc/12.txt \
        ../pandoc/13.txt \
        ../pandoc/14.txt \
        ../pandoc/15.txt \
        ../pandoc/16.txt \
        ../pandoc/17.txt \
        ../pandoc/18.txt \
        ../pandoc/19.txt \
        ../pandoc/20.txt \
        ../pandoc/21.txt \
        ../pandoc/appendix.txt

