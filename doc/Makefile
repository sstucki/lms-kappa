###
#
# Makefile -- Runs `pdflatex' to generate PDFs
#
# This is a template Makefile for generating PDFs from LaTeX sources.
# To use this file, adjust the list of targets (PDF files) through the
# `PDFS' variable.  The generic build rules and the dependency file
# generator script should take care of the rest.
#
# This file requires the `texdeps.sh' dependency file generator
# script.  If the script is not located in the same directory as this
# Makefile, you will need to adjust the `MAKEDEPS' variable.
#
# Author: Sandro Stucki <sandro.stucki@gmail.com>
#
#


### Targets (PDFs to generate) ###

PDFS = manual.pdf


### Settings ###

# Directory for storing dependency files
DEPSDIR = .deps

# Commands
LATEX = xelatex
BIBTEX = bibtex
MAKEDEPS = ./texdeps.sh


### Build rules ###

.PHONY: clean

# Make all PDFs by default.
all: $(PDFS)

# Generic rule to build PDFs: update dependency files and bibliography
# (if necessary), and re-run LaTeX another two times, so that all
# references are resolved properly.
%.pdf: %.tex
	mkdir -p "$(DEPSDIR)" \
	&& $(MAKEDEPS) "$@" "$<" > "$(DEPSDIR)/$*.P"
	$(RM) "$*.aux" "$*.bbl" \
	&& $(LATEX) "$*" \
	&& if grep -qE '\\bibdata' "$*.aux" ; \
	then $(BIBTEX) "$*" && $(LATEX) "$*"; fi \
	&& $(LATEX) "$*"

# Clean up build files.
clean:
	$(RM) $(PDFS) *.aux *.bbl *.blg *.log *.out


# Include dependency files.
-include $(PDFS:%.pdf=$(DEPSDIR)/%.P)
