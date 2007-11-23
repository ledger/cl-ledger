INIT  = $(HOME)/Library/Lisp/init.lisp
#INIT  = init.lisp
BOOT  = $(HOME)/Library/Lisp/bootstrap.lisp
#BOOT  = bootstrap.lisp
HELLO = "(format t \"Ledger server started at http://localhost:4242/~%\")"
START = "(ledger-http)"

# Running with SBCL

SBCL	  = sbcl
SBCL_CORE = sbcl.core

all: sbcl-server

sbcl-server:
	if [ -f $(SBCL_CORE) ]; then \
	    $(SBCL) --core $(SBCL_CORE) \
		    --noinform --noprint \
		    --eval $(HELLO) --eval $(START); \
	else \
	    $(SBCL) --noinform --noprint \
		    --load $(INIT) --load $(BOOT) \
		    --eval $(HELLO) --eval $(START); \
	fi

$(SBCL_CORE): $(INIT) $(BOOT)
	$(SBCL) --noinform --noprint \
		--load $(INIT) --load $(BOOT) \
		--eval "(sb-ext:save-lisp-and-die \"$@\")"

fasl: clean
	$(SBCL) --load $(INIT) --load $(BOOT) \
		--eval "(ledger)" --eval "(quit)"

# Running with CMUCL

CMUCL	   = lisp
CMUCL_CORE = cmucl.core

# jww (2007-11-22): This fails because periods won't build.

cmucl-server:
	if [ -f $(CMUCL_CORE) ]; then \
	    $(CMUCL) -core $(CMUCL_CORE) \
		-eval $(HELLO) -eval $(START); \
	else \
	    $(CMUCL) -load $(INIT) -load $(BOOT) \
		-eval $(HELLO) -eval $(START); \
	fi

cmucl-core: $(CMUCL_CORE)

$(CMUCL_CORE): $(BOOT)
	$(CMUCL) -load $(INIT) -load $(BOOT) \
		 -eval "(need-to-save-lisp-and-die \"$@\")"

x86f: clean
	$(CMUCL) -load $(INIT) -load $(BOOT) -eval "(quit)"

# Running with CMUCL

ECL = ecl

# jww (2007-11-22): This doesn't work just now because gray-streams
# won't load

ecl-server:
	$(ECL)  -load $(INIT) -load $(BOOT) \
		-eval $(HELLO) -eval $(START); \

# jww (2007-11-22): And this doesn't work because ECL can't grok SERIES.

fas:
	$(ECL) -load $(INIT) -load $(BOOT) -eval "(quit)"

# General rules

clean:
	rm -f $(SBCL_CORE) $(CMUCL_CORE)
	find . ../red-black ../periods ../cambl \
	    \(  -name '*.fasl' -o -name '*.fas' -o \
		-name '*.fsl' -o -name '*.x86f' -o \
		-name '*.xfasl' \) -type f | xargs rm -f
