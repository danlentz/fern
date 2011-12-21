;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :planks.ext)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; spin-locks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-spinlock ()
  (cons nil nil))

(defun lock-spinlock (spinlock)
  (loop while (SB-EXT:COMPARE-AND-SWAP (car spinlock) nil t)))

(defun unlock-spinlock (spinlock)
  (setf (car spinlock) nil))

(defmacro with-spinlock ((spinlock) &body body)
  (alexandria:once-only (spinlock)
    `(progn
       (lock-spinlock ,spinlock)
       (unwind-protect
            (progn ,@body)
         (unlock-spinlock ,spinlock)))))

(defun make-recursive-spinlock ()
  (cons nil 0))

(defun lock-recursive-spinlock (recursive-spinlock)
  (loop with self = SB-THREAD:*CURRENT-THREAD*
    for ret = (SB-EXT:COMPARE-AND-SWAP (car recursive-spinlock) nil self)
        until (or (null ret) (eq ret self))
        finally (incf (cdr recursive-spinlock))))

(defun unlock-recursive-spinlock (recursive-spinlock)
  (when (decf (cdr recursive-spinlock))
    (setf (car recursive-spinlock) nil)))

(defmacro with-recursive-spinlock ((recursive-spinlock) &body body)
  (alexandria:once-only (recursive-spinlock)
    `(progn
       (lock-recursive-spinlock ,recursive-spinlock)
       (unwind-protect
            (progn ,@body)
         (unlock-recursive-spinlock ,recursive-spinlock)))))

