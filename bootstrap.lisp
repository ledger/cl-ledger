(mapc 'require
      '(sb-bsd-sockets
	sb-posix
	sb-introspect
	sb-cltl2
	asdf
	asdf-install))

(dolist (libdir (directory "./lib/*/"))
  (pushnew libdir asdf:*central-registry*))

(asdf:operate 'asdf:load-op :cffi)
(push "/usr/local/lib" cffi:*foreign-library-directories*)
(push "/opt/local/lib" cffi:*foreign-library-directories*)
(push "/sw/lib" cffi:*foreign-library-directories*)

(asdf:operate 'asdf:load-op :trivial-gray-streams)
(asdf:operate 'asdf:load-op :flexi-streams)

(asdf:operate 'asdf:load-op :url-rewrite)
(asdf:operate 'asdf:load-op :rfc2388)
(asdf:operate 'asdf:load-op :cl-base64)
(asdf:operate 'asdf:load-op :chunga)
(asdf:operate 'asdf:load-op :hunchentoot)
(asdf:operate 'asdf:load-op :cl-who)

(asdf:operate 'asdf:load-op :ledger)
(asdf:operate 'asdf:load-op :ledger-textual)
(asdf:operate 'asdf:load-op :ledger-http)
