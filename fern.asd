;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(asdf:defsystem :fern
  :serial t
  :description "Efficent Tries based on OKBC project"
  :version     (:read-file-form "version.text")
  :depends-on  (:uuid)
  :in-order-to ((test-op (test-op :fern/test)))
  :components  ((:static-file  "fern.asd")
                (:static-file  "README.md")
                (:file "fern")))


(asdf:defsystem :fern/test
  :depends-on (:hu.dwim.stefil :alexandria :fern)
  :perform (asdf:test-op (o c)
             (uiop:symbol-call :fern/test
                               :run-all-tests))
  :components ((:file "test")))
