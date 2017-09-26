MAXIMA=maxima

all:
	echo 'load("sym/compile.lisp");' | $(MAXIMA)

check:
	echo ':lisp(let ((*maxima-testsdir* ".")) ($$run_testsuite))' \
	| $(MAXIMA)

doc:
	latex docsym.tex

clean:
	rm -f *.aux *.dvi *.toc *.log
	rm -f *.o

tar:
	(cd .. ; tar cvf - sym/makefile sym/*.lsp sym/*.tex sym/doconline sym/*.mac src/max_ext.lisp doc/macsym.doc doc/macsym-index.LISP ) | gzip -c > sym.tgz



