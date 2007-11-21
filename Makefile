SBCL = sbcl
CORE = sbcl.core

HELLO = "(format t \"Ledger server started at http://localhost:4242/~%\")"

all: run-server

run-server:
	if [ -f $(CORE) ]; then \
	    $(SBCL) --core $(CORE) --noinform --noprint \
		    --eval $(HELLO) \
		    --eval "(hunchentoot:start-server :port 4242)"; \
	else \
	    $(SBCL) --noinform --noprint --load bootstrap.lisp \
		    --eval $(HELLO) \
		    --eval "(hunchentoot:start-server :port 4242)"; \
	fi

core: $(CORE)

$(CORE): bootstrap.lisp
	$(SBCL) --noinform --noprint --load bootstrap.lisp \
		--eval "(sb-ext:save-lisp-and-die \"$@\")"

fasl: clean
	$(SBCL) --eval "(require 'asdf)" \
		--eval "(asdf:oos 'asdf:load-op :ledger)" \
		--eval "(quit)"

clean:
	rm -f $(CORE) *.fasl
	test -d ../red-black && rm -f ../red-black/*.fasl
	test -d ../periods && rm -f ../periods/*.fasl
	test -d ../cambl && rm -f ../cambl/*.fasl
