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

(push "src/" asdf:*central-registry*)
(asdf:operate 'asdf:load-op :cambl)

(push "test/" asdf:*central-registry*)
(asdf:operate 'asdf:load-op :cambl-test)
