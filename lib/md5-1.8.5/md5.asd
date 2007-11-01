;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          md5.asd
;;;; Purpose:       ASDF definition file for Md5
;;;; Author:        Kevin M. Rosenberg
;;;; Date Started:  Sep 2002
;;;;
;;;; $Id: md5.asd 7061 2003-09-07 06:34:45Z kevin $
;;;;
;;;; This file can be freely copied and have been placed in the public domain.
;;;; *************************************************************************

(defpackage #:md5-system (:use #:asdf #:cl))

(in-package :md5-system)

(defsystem md5
  :name "cl-md5"
  :author "Pierre Mai"
  :version "1.8"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
  :licence "Public Domain"
  :description "MD5 Message Digest function"
  :long-description "This package contains functions to compute the MD5 sum on a stream or string."
  
  :components
  ((:file "md5")))


