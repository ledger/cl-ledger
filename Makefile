SBCL = sbcl

server:
	$(SBCL) --noinform --noprint --load bootstrap.lisp \
	     --eval "(format t \"Ledger server started at http://localhost:4242/~%\")" \
	     --eval "(hunchentoot:start-server :port 4242)"
