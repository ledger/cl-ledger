(in-package :trivial-gray-streams-system)

#+cmu
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :gray-streams))

#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'stream:stream-write-string)
    (require "streamc.fasl")))

(macrolet
    ((frob ()
       (let
	   ((common-symbols
	     '(#:fundamental-stream #:fundamental-input-stream
	       #:fundamental-output-stream #:fundamental-character-stream
	       #:fundamental-binary-stream #:fundamental-character-input-stream
	       #:fundamental-character-output-stream
	       #:fundamental-binary-input-stream
	       #:fundamental-binary-output-stream #:stream-read-char
	       #:stream-unread-char #:stream-read-char-no-hang
	       #:stream-peek-char #:stream-listen #:stream-read-line
	       #:stream-clear-input #:stream-write-char #:stream-line-column
	       #:stream-start-line-p #:stream-write-string #:stream-terpri
	       #:stream-fresh-line #:stream-finish-output #:stream-force-output
	       #:stream-clear-output #:stream-advance-to-column
	       #:stream-read-byte #:stream-write-byte)))
	 `(defpackage :trivial-gray-streams
	    (:use :cl)
	    (:import-from #+sbcl :sb-gray
			  #+allegro :excl
			  #+cmu :ext
			  #+clisp :gray
			  #+openmcl :ccl
			  #+lispworks :stream
			  #-(or sbcl allegro cmu clisp openmcl lispworks) ...
			  ,@common-symbols)
	    (:export #:trivial-gray-stream-mixin
		     #:stream-read-sequence
		     #:stream-write-sequence
		     #:stream-file-position
		     ,@common-symbols)))))
  (frob))
