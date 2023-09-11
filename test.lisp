;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-

(defpackage :fern/test
  (:use :common-lisp :hu.dwim.stefil :alexandria))

(in-package :fern/test)

(defsuite* (fern :in fern))

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
    (is (equal (fern:vals x) (mapcar #'cdr pairs)))
    (is (equal (fern:keys x) (mapcar #'car pairs)))))

#|

(defun trietst (list &key (max (length list)) (check :after) (trace nil) (map nil) (keep nil))
  (ecase check
    (:after)
    (:during)
    ((nil)))
  ;; touch the list
  ;;(loop for x in list do (length x))
  (let ((some-trie (fern:make-trie)))
    (when keep (setq *trie* some-trie))
    (time
     (loop for clause in list
	   for i from 0 below max
	   for trie = (fern::find-trie clause t nil some-trie)
	   do
	   ;;(user::pp clause)
	   ;;(user::pp some-trie)
	   (when trace
	     (print "Looking for:") (pprint clause)
	     (print "Found:")	    (pprint trie))
	   (fern::set-trie-value trie
			         (if (eql (fern::trie-value trie) fern:+no-value+)
			       (list i)
			       (cons i (fern::trie-value trie))))
	   (when trace
	     (print "After inserting")
	     (pprint some-trie))
	   when (= (mod i 50) 1) do (princ ".")))
    (when (eq check :after)
      (time
      (loop for x in list
	    for i from 0 below max
	    for trie = (find-trie x nil nil some-trie)
	    when (= (mod i 50) 1) do (princ "c")
	    unless (member i (trie-value trie))
	    do (break
		"Expected ~s. Vals = ~s.  Did not find expected value for ~s"
		i (trie-value trie) x))))
    (when map
      (let ((keys nil)
            (vals nil))
        (maptrie #'(lambda (k v) (push k keys)(push v vals))
               some-trie)
        (loop
         for k in keys
         for v in vals
         for trie = (find-trie k nil nil some-trie)
         do (assert trie)
         (assert (eq (trie-value trie) v)))
	(when (not (set-equal keys (subseq list 0 max) :test #'equal))
	  (map nil #'pp keys)(break))))
    (trie-node-count some-trie)
    ))


;; things that triggerd bugs:
(trietst '(
	   (=> a)
	   (=> a b c)
	   (=> a (b c))
	   (=> a (b c) (d e) (f g))
	   (=> (b c))))
;; requires stashing a stash
(trietst '((a (b (c) (d))) (a (b f))))

(defun set-equal (x y &key (test #'eql))
  (and (subsetp x y :test test)
       (subsetp y x :test test)))

(defun trie-dbg (list indices &rest trietst-keys)
  (apply 'trietst
	 (loop for (lo hi) in indices
	       append (subseq list lo hi)) trietst-keys))





