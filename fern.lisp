;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :fern)


(in-package :fern)

(defvar *fern* nil)

(defparameter +url+             unicly:*UUID-NAMESPACE-URL*)
(defparameter +dns+             unicly:*UUID-NAMESPACE-DNS*)
(defparameter +x500+            unicly:*UUID-NAMESPACE-X500*)
(defparameter +oid+             unicly:*UUID-NAMESPACE-OID*)
(defparameter +null+            unicly::*UUID-NULL-UUID*)

(defgeneric arc (thing)  
  (:method ((uuid unique-universal-identifier))
    (coerce (uuid-get-namespace-bytes uuid) 'list)))

(defstruct (fern (:include root-trie))
  (state (make-trie-remove-duplicate-state)))

(defstruct (root-fern (:include fern))
  container)

(defun make-new-root-fern (&optional (value 0))
  (setf *fern* (make-root-fern :value value :purpose +null+ :up (arc +null+))))

(defun make-new-fern (&optional (purpose (make-v4-uuid)) (value '*no-value*)
                       (up (or *fern* (make-new-root-fern))))
  (prog1 (setf (get-hybrid-trie (arc purpose) up)
           (make-fern :value value :up up :purpose purpose))
  (incf (hybrid-trie-value up)))) 

(defun root-fern ()
  (or *fern* (make-new-root-fern)))

(define-symbol-macro |<>| (root-fern))

(defun |<>| (&rest args)
  (apply #'make-new-fern args))

#+()
(defmethod print-object ((fern fern) stream)
  (print-unreadable-object (fern stream :type t)
    (format stream
      "[~d/~d] ~A ~A ARCS, ~d NODES, ~d ELEMENTS IN-STATE"
      (fern-fronding  fern)
      (fern-seq fern)
      (trie-value (fern-trie fern))
      (if (trie-arcs-hashed-p (fern-trie fern))
        :HASHED (length (trie-arcs (fern-trie fern))))
      (trie-node-count (fern-trie fern))
      (trie-remove-duplicate-state-count (fern-state fern)))))

(defun put (thing &optional value (fern (fern)))
  (let ((val (setf (get-trie (arc thing) (fern-trie fern)) (make-fern (or value thing)))))
    (cl:values val thing (incf (fern-seq fern)))))

(defun get (thing &optional (fern (fern)))
  (get-trie (arc thing) (fern-trie fern)))

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
