all:
	xelatex paper.tex

bib:
	xelatex paper.tex && bibtex paper.aux && xelatex paper.tex && xelatex paper.tex

clean:
	rm -f paper.pdf
	rm -f paper.aux
	rm -f paper.log
	rm -f paper.dvi
	rm -f paper.log
	rm -f paper.bbl
	rm -f paper.out
	rm -f paper.blg
	rm -rf auto
	rm -rf texput.log
