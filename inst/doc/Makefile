
QUIET=--quiet

# Strip .Rnw from all vignette files
FILES=${subst .Rnw, , ${shell ls *.Rnw}}

# Add .pdf to all vignette files
default: ${addsuffix .pdf, ${FILES}} # Compile all vignette files

%.tex: %.Rnw # Weave Sweave files
	R CMD Sweave $<

%.pdf: %.tex # Compile LaTeX files
	texi2dvi --pdf --clean ${QUIET} $<
	make clean

clean: # Remove "compilation" files
	@rm -f *.{aux,log,out,bbl,blg}
	@rm -f ${addsuffix .tex, ${FILES}}
	@rm -f *-plot.{pdf,eps} Rplots.ps

distclean: clean # Remove also PDF files
	@rm ${addsuffix .pdf, ${FILES}}

debug: # Compile LaTeX files in debug mode
	make QUIET=''

help: # Help
	@echo -e '\nTarget: Dependency # Description'; \
	echo '=================================================='; \
	egrep '^[[:alnum:].+_()]*:' ./Makefile
