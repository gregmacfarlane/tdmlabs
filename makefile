## Sweave Document MAKE Compiler ##

# for Mac OSX and Linux

# Josephine D. Kressner
# Georgia Institute of Technology
# josiekressner@gatech.edu

# Edits and annotation by Greg Macfarlane
# gmacfarlane7@gatech.edu

# Enter the name of the final .pdf document here:
MASTER = Homework.pdf
# Your principal .rnw or .tex file should have
# the same stem. If your principal file is
# foo.rnw, set MASTER = foo.pdf

# This file is designed for UNIX- type
# operating systems and successfully compiles
# on Mac OS X and Red Hat Linux, at least. Compiling on
# Windows will require different LaTeX output
# suppressors (>/dev/null will not be recognized)


RNWFILES = $(wildcard *.rnw) 
TEXFILES = $(wildcard *.tex)
DEPENDS = $(patsubst %.rnw,%.tex,$(RNWFILES)) $(TEXFILES)


all: $(MASTER) 
	@ open $<
	@ make clean

$(MASTER): $(DEPENDS) 

%.tex: %.rnw
	@echo + Sweaving $@ from $< ... 
	knit -n '$<' 	

%.pdf: %.tex 
	@ echo + Writing $@ from $< ...
	@ texi2pdf '$<' >/dev/null
clean:
	@ echo + Cleaning ...
	@ rm -f *.aux *.lof *.log *.lot *.toc Rplots.pdf 
	@ rm -f *.bbl *.blg *.dvi 
	@ rm -f $(patsubst %.rnw,%.tex,$(RNWFILES)) 

realclean: clean
	@echo + Really cleaning ...
	rm -f $(MASTER)


Stangle:$(RNWFILES)
	R64 CMD STANGLE '$<'
	open *.R -a R64 


menu:
	@ echo + ==============================
	@ echo + .......GNU Make menu..........
	@ echo + all: ......... create document
	@ echo + clean: ...... delete aux files
	@ echo + realclean: . delete all output
	@ echo + Stangle: extract R code into R
	@ echo + 
	@ echo + Georgia Tech---------
	@ echo + --------Civil Engineering
	@ echo + ==============================
