;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-

(defpackage :fern/test
  (:use :common-lisp :hu.dwim.stefil :alexandria)
  (:export :run-all-tests :fern))

(in-package :fern/test)

(defsuite* (fern.test :in root-suite))

(defun run-all-tests ()
  (funcall-test-with-feedback-message 'fern.test))

(defparameter *fern* nil)

(defparameter *atoms* `(a b :c 42 "foo" ,(gensym) 0.99 :d #\e))

(defun random-atom ()
  (random-elt *atoms*))

(defun random-expression (&optional (depth 0))
  (let ((key (random (+ depth 3))))
    (case key
      (0 (list (random-expression (+ depth 1))
	       (random-expression (+ depth 1))))
      (1 (cons (random-expression (+ depth 1))
	       (random-expression (+ depth 1))))
      (otherwise (random-atom)))))

(defun random-arc (&optional (f #'uuid:make-v1-uuid))
  (let ((uuid (funcall f)))
    (values (coerce (uuid:uuid-to-byte-array uuid) 'list) uuid)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Correctness
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun correctness-test-with-generators (key-fn val-fn &optional (n 500) (quiet-p t))
  (let ((fern (fern:make))
	(facts-so-far (make-hash-table :test #'equal)))
    (setf *fern* fern)
    (loop for i from 1 to n
          for key = (funcall key-fn)
	  for value = (funcall val-fn)
	  collect key into keys
	  do (setf (fern:get key fern) value)
	     (setf (gethash key facts-so-far) value)
	  (let (found list-of-facts)
	    (fern:map #'(lambda (k v)
		          (when (not quiet-p) (format t "~%~S:	~S" k v))
		          (push (list k v) found)) fern)
	    (maphash #'(lambda (k v)
                         (push (list k v) list-of-facts)) facts-so-far)
	    (let ((diff1 (set-difference list-of-facts found :test #'equal))
		  (diff2 (set-difference found list-of-facts :test #'equal)))
	      (when diff1 (format t "~%;;; Not found: ~{~%~S~}" diff1))
	      (when diff2 (format t "~%;;; Shouldn't have found: ~{~%~S~}" diff2))
	      (is (not (or diff1 diff2))))))))

(deftest correctness-test ()
  (correctness-test-with-generators #'random-expression #'random-expression)     ;; expression structured arcs
  (correctness-test-with-generators #'uuid:make-v4-uuid #'random-atom)           ;; distinct atomic arcs
  (correctness-test-with-generators (lambda () (random-arc #'uuid:make-v4-uuid)) ;; distinct tuple arcs
                                    #'random-atom)
  (correctness-test-with-generators #'random-arc #'random-atom)                  ;; Deeply shared tuple arcs
  (correctness-test-with-generators #'random-atom #'uuid:make-v4-uuid))          ;; Many overwriten arcs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stress
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun stress-test-with-generator (key-fn n)
  (let ((some-fern (fern:make)) keys misses)
    (loop for clause = (funcall key-fn)
	  for i from 1 to n
          do (push i (fern::get-trie clause some-fern))
          do (push clause keys))
    (loop for x in keys
	  for i downfrom n
	  unless (member i (fern:get some-fern x))
          do (push (cons x i) misses))
    (is (not misses))))

(deftest stress-test ()
  (stress-test-with-generator #'random-arc        5000)
  (stress-test-with-generator #'random-atom       50000)
  (stress-test-with-generator #'random-expression 50000)
  (stress-test-with-generator #'uuid:make-v4-uuid 500000)
  (stress-test-with-generator (lambda () (random-arc #'uuid:make-v4-uuid)) 500000))


(deftest mapping-test ()
  (let* (acc (x (fern:make))
         (pairs (loop for i from 1 to 100000
                     for j from 10 downto 1
                     do (setf (fern:get i x) j)
                      collect (cons i j))))
    (is (equal (fern::vals x) (mapcar #'cdr pairs)))
    (is (equal (fern::keys x) (mapcar #'car pairs)))))
