(mapc 'require
      '(sb-bsd-sockets
	sb-posix
	sb-introspect
	sb-cltl2
	asdf
	asdf-install))

(push "lib/red-black/" asdf:*central-registry*)
(asdf:operate 'asdf:load-op :rbt-trees-struct)

(defmacro load-or-install (package)
  `(handler-case
       (progn
	 (asdf:operate 'asdf:load-op ,package))
     (asdf:missing-component ()
       (asdf-install:install ,package))))

(load-or-install :xlunit)
(load-or-install :cl-ppcre)

(load-or-install :md5)
(load-or-install :cffi)
(push "/usr/local/lib" cffi:*foreign-library-directories*)
(load-or-install :trivial-gray-streams)
(load-or-install :flexi-streams)
(load-or-install :url-rewrite)
(load-or-install :rfc2388)
(load-or-install :cl-base64)
(load-or-install :chunga)
(push  :hunchentoot-no-ssl *features*)
(load-or-install :hunchentoot)
(load-or-install :cl-who)

(sb-ext:save-lisp-and-die "sbcl.prep-core")
