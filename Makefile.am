SUBDIRS	      = #ecl
BUILT_SOURCES =
CLEANFILES    =
EXTRA_DIST    = LICENSE \ # doc test contrib scripts
		acprep # TODO

ESC_srcdir=`echo "$(srcdir)" | sed 's/\//\\\\\//g'`
ESC_builddir=`echo "$(top_builddir)" | sed 's/\//\\\\\//g'`
ESC_distdir=`echo "$(distdir)" | sed 's/\//\\\\\//g'`

dist-hook:
	rm -fr `find $(distdir) -name .svn`

###############################################################################

bin_PROGRAMS = ledger

ledger_SOURCES  = src/hello.lisp

nodist_info_TEXINFOS = doc/ledger.texi

#ECL = $(ESC_builddir)/ecl-0.9i/build/bin/ecl
ECL = ecl
SBCL = sbcl

.lisp.o:
	$(ECL) -s -o $@ -compile $< 

ledger$(EXEEXT): $(ledger_OBJECTS) $(ledger_DEPENDENCIES)
	$(ECL) -eval "(require 'cmp)" \
	       -eval "(let ((args (nthcdr 8 (ext:command-args)))) \
                         (c:build-program (first args) :lisp-files (rest args)))" \
	       -eval "(quit)" -- $@ $(ledger_OBJECTS)

sbcl.ledger-core: bootstrap.lisp
	$(SBCL) --load bootstrap.lisp

core: sbcl.ledger-core

check: sbcl.ledger-core
	$(SBCL) --core sbcl.ledger-core --noinform --noprint \
	     --eval "(format t \"Running CAMBL unit tests...~%\")" \
	     --load "src/cambl" \
	     --load "test/cambl-test" \
	     --eval "(quit)"

server: sbcl.ledger-core
	$(SBCL) --core sbcl.ledger-core --noinform --noprint \
	     --load "src/cambl" \
	     --load "src/ledger" \
	     --load "src/ledger-server" \
	     --eval "(format t \"Ledger server started at http://localhost:4242/~%\")" \
	     --eval "(hunchentoot:start-server :port 4242)"

DISTCLEANFILES = sbcl.ledger-core

###############################################################################

#dist_lisp_LISP = lisp/ledger.el lisp/timeclock.el

#DISTCLEANFILES = ledger.elc timeclock.elc

###############################################################################

#TESTS = UnitTests
#
#check_PROGRAMS = $(TESTS)
#
#nodist_UnitTests_SOURCES = tests/UnitTests.cc \
#	\
#	tests/utility/t_utils.cc      \
#	tests/utility/t_times.cc      \
#	tests/numerics/t_commodity.cc \
#	tests/numerics/t_amount.cc    \
#	tests/numerics/t_balance.cc
#
#fullcheck: check
#	MallocGuardEdges=1	 \
#	MallocScribble=1	 \
#	MallocPreScribble=1	 \
#	MallocCheckHeapStart=100 \
#	MallocCheckHeapEach=100	 \
#	DYLD_INSERT_LIBRARIES=/usr/lib/libgmalloc.dylib \
#	$(srcdir)/valgrind.sh $(top_builddir)/UnitTests$(EXEEXT) --verify

###############################################################################

alldocs: doc/ledger.info doc/ledger.pdf

###############################################################################

clean-backupfiles:
	rm -fr *~  \
	       .*~ \
	       .\#*

clean-documentation:
	(cd docs; \
	rm -fr *.aux  \
	       *.cp   \
	       *.fn   \
	       *.info \
	       *.ky   \
	       *.log  \
	       *.pdf  \
	       *.pg   \
	       *.toc  \
	       *.tp   \
	       *.vr)

clean-buildproducts:
	rm -fr *.Plo  \
	       *.Po   \
	       *.a    \
	       *.elc  \
	       *.gcno \
	       *.gdca \
	       *.la   \
	       *.lo   \
	       *.o    \
	       *.so   \
	       .deps  \
	       .libs  \
	       build

clean-debugdata:
	rm -fr .gdb_history \
	       TAGS         \
	       gmon.out	    \
	       h	    \
	       out

clean-autoconf:
	(cd $(srcdir);
	rm -fr Makefile	     \
	       Makefile.in   \
	       acconf.h	     \
	       acconf.h.in   \
	       aclocal.m4    \
	       autom4te      \
	       compile	     \
	       config.guess  \
	       config.sub    \
	       configure     \
	       depcomp	     \
	       elc-stamp     \
	       elc-temp	     \
	       elisp-comp    \
	       install-sh    \
	       libtool	     \
	       ltconfig	     \
	       ltmain.sh     \
	       missing	     \
	       mkinstalldirs \
	       py-compile    \
	       stamp	     \
	       texinfo.tex   \
	       ylwrap)

all-clean: maintainer-clean    \
	   clean-buildproducts \
	   clean-backupfiles   \
	   clean-debugdata     \
	   clean-documentation \
	   clean-autoconf
