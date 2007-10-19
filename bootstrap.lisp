(mapc 'require '(sb-bsd-sockets sb-posix sb-introspect sb-cltl2 asdf))

(load "lib/red-black/rbt-trees-package.lisp")
(load "lib/red-black/rbt-types.lisp")
(load "lib/red-black/red-black-trees-struct.lisp")

(push "lib/xlunit-0.6.2/" asdf:*central-registry*)
(asdf:operate 'asdf:load-op :xlunit)

(push "lib/cl-ppcre-1.3.2/" asdf:*central-registry*)
(asdf:operate 'asdf:load-op :cl-ppcre)

(push "lib/md5-1.8.5/" asdf:*central-registry*)
(asdf:operate 'asdf:load-op :md5)

(push "lib/cffi-0.9.2/" asdf:*central-registry*)
(asdf:operate 'asdf:load-op :cffi)
(push "/usr/local/lib" cffi:*foreign-library-directories*)
(push "lib/trivial-gray-streams-2006-09-16/" asdf:*central-registry*)
(asdf:operate 'asdf:load-op :trivial-gray-streams)
(push "lib/flexi-streams-0.13.1/" asdf:*central-registry*)
(asdf:operate 'asdf:load-op :flexi-streams)
(push "lib/url-rewrite-0.1.1/" asdf:*central-registry*)
(asdf:operate 'asdf:load-op :url-rewrite)
(push "lib/rfc2388/" asdf:*central-registry*)
(asdf:operate 'asdf:load-op :rfc2388)
(push "lib/cl-base64-3.3.2/" asdf:*central-registry*)
(asdf:operate 'asdf:load-op :cl-base64)
(push "lib/chunga-0.4.1/" asdf:*central-registry*)
(asdf:operate 'asdf:load-op :chunga)
(push "lib/cl-who-0.11.0/" asdf:*central-registry*)
(asdf:operate 'asdf:load-op :cl-who)
(push "lib/hunchentoot-0.14.3/" asdf:*central-registry*)
(push  :hunchentoot-no-ssl *features*)
(asdf:operate 'asdf:load-op :hunchentoot)

(push "src/" asdf:*central-registry*)
(asdf:operate 'asdf:load-op :cambl)

;(push "test/" asdf:*central-registry*)
;(asdf:operate 'asdf:load-op :cambl-test)

(sb-ext:save-lisp-and-die "sbcl.core")
