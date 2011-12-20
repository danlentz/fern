;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :fern)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ferns proper
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *fern* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ARCs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +url+             unicly::*UUID-NAMESPACE-URL*)
(defparameter +dns+             unicly::*UUID-NAMESPACE-DNS*)
(defparameter +x500+            unicly::*UUID-NAMESPACE-X500*)
(defparameter +oid+             unicly::*UUID-NAMESPACE-OID*)
(defparameter +null+            unicly::*UUID-NULL-UUID*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric arc (thing)
  (:method ((uuid integer))
    (coerce (unicly::uuid-integer-128-to-byte-array uuid) 'list))
  (:method ((uuid vector))
    (coerce uuid 'list))
  (:method ((uuid string))
    (coerce (remove #\- uuid) 'list))
  (:method ((uuid unique-universal-identifier))
    (coerce (uuid-get-namespace-bytes uuid) 'list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Well Known Reference Arcs (in "canonical" byte-array representation)
;;
;; (arc +null+) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;; (arc +url+)  (107 167 184 17 157 173 17 209 128 180 0 192 79 212 48 200)
;; (arc +oid+)  (107 167 184 18 157 173 17 209 128 180 0 192 79 212 48 200)
;; (arc +dns+)  (107 167 184 16 157 173 17 209 128 180 0 192 79 212 48 200)
;; (arc +x500+) (107 167 184 20 157 173 17 209 128 180 0 192 79 212 48 200)
;;
;;; Integer-based ARCs allow sequential ordering and incremental
;;; allocation of namespace
;;
;; (arc 0000) => (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;; (arc 0001) => (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
;; (arc 0002) => (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2)
;; (arc 9999) => (0 0 0 0 0 0 0 0 0 0 0 0 0 0 39 15)
;;
;;; Bit Vectors can support maximal fan-out with minimization of storage
;;; by use of delinearized *stash* 
;;
;; (arc (uuid-to-bit-vector +oid+))
;; (0 1 1 0 1 0 1 1 1 0 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 1 0 0 1 0 1 0 0 1 1 1 0 1 1 0 1 0 1 1 0 1
;;  0 0 0 1 0 0 0 1 1 1 0 1 0 0 0 1 1 0 0 0 0 0 0 0 1 0 1 1 0 1 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0
;;  0 1 0 0 1 1 1 1 1 1 0 1 0 1 0 0 0 0 1 1 0 0 0 0 1 1 0 0 1 0 0 0)
;;
;; (arc (uuid-to-bit-vector +x500+))
;; (0 1 1 0 1 0 1 1 1 0 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 1 0 1 0 0 1 0 0 1 1 1 0 1 1 0 1 0 1 1 0 1
;;  0 0 0 1 0 0 0 1 1 1 0 1 0 0 0 1 1 0 0 0 0 0 0 0 1 0 1 1 0 1 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0
;;  0 1 0 0 1 1 1 1 1 1 0 1 0 1 0 0 0 0 1 1 0 0 0 0 1 1 0 0 1 0 0 0)
;;
;;; this is a slightly inefficent encoding, but it provides a useful arc densitity in that
;;; it will never require any hash based nodes. should be converted to nybble (4-bit hex)
;;
;; (arc (princ-to-string +x500+))
;; (#\6 #\b #\a #\7 #\b #\8 #\1 #\4 #\9 #\d #\a #\d #\1 #\1 #\d #\1 #\8 #\0 #\b #\4 #\0 #\0 #\c
;;  #\0 #\4 #\f #\d #\4 #\3 #\0 #\c #\8)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defstruct (dynamic-state (:include trie-remove-duplicate-state)))
(defstruct (fern          (:include root-trie))
  (local-state   (make-dynamic-state))
  (compute-frond (list
                              :byte-array  #'identity
                              :bit-vector  #'uuid-to-bit-vector
                              :hex-digit   #'princ-to-string
                              :random-id   #'make-v4-uuid)))
             
(defstruct (root-fern     (:include fern))
  container)

(defun root-fern (&optional (purpose (short-site-name)))
  (or (and (boundp '*fern*) (symbol-value '*fern*))
    (setf (symbol-value '*fern*) (make-root-fern :purpose purpose :up +url+))))

(defun fern (&optional purpose context)
  (let* ((purpose (or purpose (princ-to-string (make-v4-uuid))))
          (context (or context (root-fern)))
          (parent-id (make-v5-uuid (root-trie-up context) (root-trie-purpose context)))
          (local-id  (make-v5-uuid parent-id purpose))         
          (baby (make-fern :up parent-id :purpose purpose)))
    (set-hybrid-trie-value baby local-id)
    (if (not (get-hybrid-trie (arc local-id) context))
      (setf (get-hybrid-trie (arc local-id) context) baby))))

;;      (set-hybrid-trie-value trie baby))
;    trie))
      

(define-symbol-macro |<>| (root-fern))

(defun |<>| (&rest args)
  (apply #'fern args))

;; (<> (short-site-name))
;;
;; (defun make-new-root-fern (&optional (value 0) (context +url+) (up +null+))
;; (make-root-fern :value value :purpose context :up up))
;; (defun make-new-fern (&optional (value '*no-value*) (context (make-v4-uuid))
;; (up +url+)  (make-fern))

 (defmethod print-object ((fern root-fern) stream)
   (print-unreadable-object (fern stream :type t)
     (format stream "~A :UP ~A" (princ-to-string (root-trie-purpose fern)) nil)))

 (defmethod print-object ((fern fern) stream)
   (print-unreadable-object (fern stream :type t)
     (format stream "~A :UP ~A" (princ-to-string (root-trie-purpose fern))
       (root-trie-up fern))))
  
(defun list-all-root-ferns ()
  (hybrid-trie-all-values (root-fern)))

(defun make-frond-injector (fern)
  "Given a FERN, return an injector function accepting KEY and VALUE."
  (lambda (k v) (setf (get-hybrid-trie k fern) v)))

(defmacro apply-key (key element)
  `(if ,key (funcall ,key ,element)
     ,element))

(defmacro satisfies-the-test (elt)
  (alexandria:with-gensyms (key-tmp)
    `(let ((,key-tmp (apply-key key ,elt)))
       (cond (testp (funcall test ,key-tmp))
             (notp (not (funcall test-not ,key-tmp)))
         (t t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General API / end-user interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fern:put (arc value &optional (fern (root-fern)))
  (let* ((frond  (new-root-hybrid-trie value fern))
         (result (setf (get-hybrid-trie (arc arc) fern) frond)))
    (cl:values result (arc arc) fern)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fern:get (&optional (fern (fern)) arc)
  (get-hybrid-trie (or arc +null+) fern))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get* (thing &optional (fern (fern)))
  (get-trie-returning-node (arc thing) (fern-trie fern)))

(defun drop (thing &optional (fern (fern)))
  (prog1 (delete-trie (arc thing) (fern-trie fern))
    (incf (fern-seq fern))))

(defun map (fn/2 &optional (fern (fern)))
  (maptrie fn/2 (fern-trie fern)))

(defun map-values (fn &optional (fern (fern)))
  (maptrie-values fn (fern-trie fern)))

(defun map-nodes (fn &optional (fern (fern)))
  (maptrie-nodes fn (fern-trie fern)))

(defun map-nodes-remove-if (fn &optional (fern (fern)))
  (maptrie-nodes-remove-if fn (fern-trie fern)))

(defun all-values (&optional (fern (fern)))
  (trie-all-values (fern-trie fern)))

(defun get-value (&optional (fern (fern)))
  (trie-value (fern-trie fern)))

(defun set-value (value &optional (fern (fern)))
  (prog1 (set-trie-value (fern-trie fern) value)
    (incf (fern-seq fern))))

(defun deep-copy (&optional (fern (fern)))
  (copy-deep-trie (fern-trie fern)))

#|


(defun |<>| (&optional control-id &key (base-namespace *site-context*))
  (if control-id
    (make-fern            (concatenate 'string
                                          (typecase base-namespace
                                            (object (object-identity-control-id base-namespace))
                                            (unique-universal-identifier ""))            
                                          control-id)
      (typecase base-namespace
                        (object (object-identity-uuid base-namespace))
#+()                        (string (make-object
                                  :base-namespace *site-context*
                                  :control-id string))
                        (unique-universal-identifier base-namespace)
                        (null *global-context*)))

    base-namespace))
|#
;; (make-fern "uuu")

;; (<> "x/")
;;
;; #<OBJECT 
;;    :OBJECT-IDENTITY-CONTROL-ID  "http://ebu.gs/x/"
;;    :OBJECT-IDENTITY-UUID        faf677b9-e666-5993-90a1-e96730ac0dd9 >

  
#|
(defparameter +literal+
  (make-object
    :base-namespace +url+
    :control-id "http://www.w3.org/2000/01/rdf-schema#Literal"))
  
(defparameter +statement+
  (make-object
    :base-namespace +url+
    :control-id "http://www.w3.org/1999/02/22-rdf-syntax-ns#Statement"))

|#
