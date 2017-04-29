
all: dsf_2017.pdf

%.pdf: %.tex
	latexmk -pdf -pdflatex="xelatex" -use-make $<

clean:
	latexmk -CA

