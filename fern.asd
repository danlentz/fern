;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-user)

(asdf:defsystem :fern
  :serial t
  :depends-on (:unicly)
  :components ((:file "package")
                (:file "trie")
                (:file "object")
                (:file "fern")))

