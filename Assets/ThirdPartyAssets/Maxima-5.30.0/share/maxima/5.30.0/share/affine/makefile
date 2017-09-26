.SUFFIXES: .o .c .lisp
PUBLIC=/home/wfs
MAXIMA=maxima

SRC = ${PUBLIC}/maxima/src
SRC = /usr/local/src/local/maxima-5.2/src
SAVEDKCLDIR =/usr/local/src/local/gcl-2.2/unixport
MAXDIR=/usr/local/src/local/maxima-5.2

OBJS=	amacros.o dummy-gc.o polybas.o sparsemat.o \
	aquotient.o new-rat.o    polya.o \
	polyb.o \
	polysmp.o sub-proj.o polyc.o polyd.o sheafa.o sheafb.o sheafc.o \
	dim-3.o ndotsimp.o modsimp.o todd-coxeter.o

.PRECIOUS: ${OBJS}


#	echo `#` > mcom
#	echo "$(MAXDIR)/src/saved_max_compile $(MAXDIR)/src/" >> mcom
all:	
#	chmod a+x mcom
	make objs
#	echo '(load "sysdef.lisp")' \
#	'(make::make :affine)(si::save-system "saved_affine")' | ${MAXIMA}

$(OBJS):  
	@date
	echo 'to_lisp(); (load "sysdef.lisp")' \
	'(load "${MAXDIR}/src/SYS-PROCLAIM.lisp")(load "sys-proclaim.lisp")' \
	'(load "${MAXDIR}/src/optimize")' \
	'(proclaim '\''(optimize (safety 0) (speed 2) (space 2)))' \
	'(make:make :affine :compile t)' | ${MAXIMA}

saved_affine:	$(OBJS)
	if [ -f init.lsp ] ; then  mv init.lsp xinit.lsp ; fi
	echo 'to_lisp();(load "sysdef.lisp")(make:make     :affine )' \
	'(setq $$file_search '  "\"$(MAXDIR)/src/foo.o\")" \
	'#+sgc(si::sgc-on nil)(gbc nil)(in-package :maxima)(load "dim4-help.lisp")($$batchload "macbasic.mac")(gbc nil) #+sgc #+no(si::sgc-on t)(si::save-system "saved_affine")' | ${MAXIMA}
	if [ -f xinit.lsp ] ;then mv xinit.lsp init.lsp ; fi

	
objs:   $(OBJS)

make.o :
	 $(SAVEDKCLDIR)/saved_kcl $(SAVEDKCLDIR) $*.lisp $* S1000

recompile:
	make clean
	make


clean:
	rm -f $(OBJS)

tags:	
	echo "etags $(OBJS)" | sed -e s:\\\.o:\\\.lisp:g | sh

OTHERS=macbasic.mac dim4-help.lisp makefile sysdef.lisp make.lisp README sys-proclaim.lisp



compressed:
	tar cvf - `echo $(OBJS) | sed -e s:\\\.o:\\\.lisp:g` $(OTHERS) | gzip  -c  > ../affine.tgz

test:
	make compressed
	rm -fr ../tmp/affine
	mkdir ../tmp/affine
	(cd ../tmp/affine ; uncompress -c ../../affine.tar.Z | tar xf - ; make)


.lisp.o::
	@date
	echo '(load "sysdef.lisp")' \
	'(proclaim '\''(optimize (safety 0) (speed 2) (space 2)))' \
	'(make:make :affine :compile t)' | ${MAXIMA}

sys-proclaim.lisp: 
	echo "(load \"${SAVEDKCLDIR}/../cmpnew/collectfn.o\")" \
	"(load \"${MAXDIR}/src/SYS-PROCLAIM.lisp\")" \
	"(load \"${SAVEDKCLDIR}/../lsp/sys-proclaim.lisp\")" \
	'(in-package "COMPILER")(make-all-proclaims "*.fn")' | ${MAXIMA}
	
	










	