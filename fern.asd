;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-user)


(asdf:defsystem :fern
  :serial t
  :depends-on (:unicly :planks :hh-redblack :clos-diff :contextl :named-readtables)
  :components ((:file "package")
                (:file "trie")
;;                (:file "fern")
;;                (:file "lock")
;;                (:file "streams")
;;                (:file "planks")
;;                (:file "context")
                #+()
                (:file "object")))

(flet ((check-graph-host ()
         (not (null (probe-file (translate-logical-pathname #p"graph:"))))))
  (unless (check-graph-host)
    (let ((default-graph-base-directory (merge-pathnames "graph/" (user-homedir-pathname))))
      (warn "Logical Host GRAPH not defined. Assigning default translation ~A"
        default-graph-base-directory)
      (ensure-directories-exist default-graph-base-directory)
      (setf (logical-pathname-translations "GRAPH")
        `(("**;*.*.*" ,(merge-pathnames "**/*.*" default-graph-base-directory)))))
    (assert (check-graph-host))))
