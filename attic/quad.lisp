;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-user)
(defpackage :quad (:use :cl))
(in-package :quad)

(defun qar (q) (car q))
(defun (setf qar) (v q) (setf (car q) v))

(defun qbr (q) (cadr q))
(defun (setf qbr) (v q) (setf (cadr q) v))

(defun qcr (q) (caddr q))
(defun (setf qcr) (v q) (setf (caddr q) v))

(defun qdr (q) (cdddr q))
(defun (setf qdr) (v q) (setf (cdddr q) v))

(defun sub-quads (q)
  (loop for childq on (qcr q) by #'qdr
      collecting childq))

(defun sub-quads-do (q fn)
  (loop for childq on (qcr q) by #'qdr
      do (funcall fn childq)))

(defun quad-traverse (q fn &optional (depth 0))
  (funcall fn q depth)
  (sub-quads-do q
    (lambda (subq)
      (quad-traverse subq fn (1+ depth)))))

(defun quad (operator parent contents next)
  (list operator parent contents next))

(defun quad* (operator parent contents next)
  (list operator parent contents next))

(defun qups (q)
  (loop for up = (qbr q) then (qbr up)
        unless up do (loop-finish)
        collecting up))

(defun quad-tree (q)
  (list* (qar q)
    (loop for childq on (qcr q) by #'qdr
        while childq
          collecting (quad-tree childq))))

(defun tree-quad (tree &optional parent)
  (let* ((q (quad (car tree) parent nil nil))
         (kids (loop for k in (cdr tree)
                     collecting (tree-quad k q))))
    (loop for (k n) on kids
          do (setf (qdr k) n))
    (setf (qcr q) (car kids))
    q))

#+test
(test-qt)

(defun test-qt ()
  (print (quad-tree #1='(zot nil (foo #1# ("123" "abc")
                                . #2=(bar #1# (ding #2# "456"
                                                dong #2# "789")))))))
#+()
(print #1='(zot nil (foo #1# ("123" "abc")
                          . #2=(bar #1# (ding #2# "456"
                                          dong #2# "789")))))
#+xxxx
(test-tq)

(defun test-tq ()
  (let ((*print-circle* t)
        (tree '(zot (foo ("123")) (bar (ding) (dong)))))
    (assert (equal tree (quad-tree (tree-quad tree))))))



(defun testq ()
  (let ((*print-circle* t))
    (let ((q #1='(zot nil (foo #1# ("123" "abc")
                            . #2=(bar #1# (ding #2# "456"
                                            dong #2# "789"))))))
      (print '(traverse showing each type and data preceded by its depth))
      
      (quad-traverse q (lambda (q depth)
                         (print (list depth (qar q)(qcr q)))))
      (print `(listify same ,(quad-tree q))))
    (let ((q #3='(zot nil (ding #3# "456"
                                  dong #3# "789"))))
      (print '(traverse showing each "car" and itd parentage preceded by its depth))
      (print '(of data (zot (ding (dong)))))
      (quad-traverse q (lambda (q depth)
                         (print (list depth (qar q)
                                  (mapcar 'qar (qups q)))))))))

;;;(defun tree-quad (tree)
  t

(defun testq2 ()
  (let ((*print-circle* t))
    (let ((q #2='(zot nil (ding #2# "456"
                            dong #2# "789"))))
      (print '(traverse showing each "car" and itd parentage preceded by its depth))
      (print '(of data (zot (ding (dong)))))
      (quad-traverse q (lambda (q depth)
                         (print (list depth (qar q)
                                  (mapcar 'qar (qups q)))))))))


              
  
