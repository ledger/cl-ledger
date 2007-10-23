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

.lisp.o:
	$(ECL) -s -o $@ -compile $< 

ledger$(EXEEXT): $(ledger_OBJECTS) $(ledger_DEPENDENCIES)
	$(ECL) -eval "(require 'cmp)" \
	       -eval "(let ((args (nthcdr 8 (ext:command-args)))) \
                         (c:build-program (first args) :lisp-files (rest args)))" \
	       -eval "(quit)" -- $@ $(ledger_OBJECTS)

sbcl.prep-core: prepcore.lisp
	sbcl --load prepcore.lisp

check: runpreptests.lisp src/cambl.lisp test/cambl-test.lisp sbcl.prep-core
	sbcl --core sbcl.prep-core --load runpreptests.lisp

DISTCLEANFILES = sbcl.prep-core

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
