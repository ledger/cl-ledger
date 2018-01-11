LISP ?= sbcl
asdf_system := cl-ledger

all:
	${LISP} \
		--load "${asdf_system}.asd" \
		--eval "(asdf:make \"${asdf_system}\")" \
		--eval "(uiop:quit)"

clean:
	rm -f cl-ledger
