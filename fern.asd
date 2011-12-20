;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-user)


(asdf:defsystem :fern
  :serial t
  :depends-on (:unicly :planks :hh-redblack :clos-diff :contextl)
  :components ((:file "package")
                (:file "trie")
                (:file "fern")
                (:file "lock")
                (:file "streams")
                (:file "planks")
                (:file "context")
                #+()
                (:file "object")))

