;;; -*- mode: lisp -*-

(defpackage :trivial-gray-streams-system
(:use :cl :asdf))
(in-package :trivial-gray-streams-system)

(defsystem :trivial-gray-streams
  :serial t
  :components ((:file "package") (:file "mixin")))
