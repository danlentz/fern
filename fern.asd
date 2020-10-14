;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;


(defsystem :fern
  :serial t
  :depends-on (:uuid)
  :components ((:file "package")
                (:file "fern")))
