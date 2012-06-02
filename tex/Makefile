PDFLATEX = xelatex -shell-escape

PNAME=notes-all

LOCALE=ru_RU.koi8r

.PHONY: all
all: $(SYNTAXTXT) $(SYNTAXTEX) $(PNAME).pdf

$(PNAME).ind: $(PNAME).idx
	iconv -f utf-8 -t windows-1251 < $< > $(PNAME)-tmp.idx
	makeindex -s $(PNAME).ids $(PNAME)-tmp
	iconv -f windows-1251 -t utf-8 < $(PNAME)-tmp.ind > $@.tmp
	rm -f $(PNAME)-tmp.*
	mv $@.tmp $@

$(PNAME).pdf: *.tex Makefile
	$(PDFLATEX) $(PNAME)
	make $(PNAME).ind
	$(PDFLATEX) $(PNAME)
#	while grep 'Label(s) may have changed' $(PNAME).log > /dev/null 2>&1 ; do \
#		$(PDFLATEX) $(PNAME) ; \
	done

.PHONY: 1
1: $(SYNTAXTXT) $(SYNTAXTEX)
	$(PDFLATEX) $(PNAME)
	make $(PNAME).ind

.PHONY: stat
stat:
	@echo "Overfulls: `grep ^Overfull tapl.log | wc -l`"
	@echo "Underfulls: `grep ^Underfull tapl.log | wc -l`"

clean:; rm -f *.aux *.lof *.log *.lot $(PNAME).bbl *.dvi $(PNAME).blg *.toc $(PNAME).pdf \
	$(PNAME).idx $(PNAME).ilg $(PNAME).ind $(PNAME).out

