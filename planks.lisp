;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :fern)

(arnesi:eval-always 
  (ql:quickload   :hh-redblack)
  (ql:quickload   :planks)
  (rename-package :planks.btree :planks '(:planks.btree))
  (use-package  (rename-package :hh-redblack :hh-redblack '(:hh)) :fern))


(defun blank-identity ()
  (princ-to-string (make-v4-uuid)))


(defun btree-list ()
  (let (trees)
    (maphash #'(lambda (k v) (push (cons k v) trees))
      planks.btree::*btrees*)
    trees))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BTREE-CLASS: light-weight metaobject managed persistence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; persistent-heap-storage-btree 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun ensure-btree (btree-pathname &rest args)
  (unless (probe-file btree-pathname)
    (apply #'planks.btree:make-btree (push btree-pathname args))))



(defun ensure-heap (btree-pathname &rest args)
  (let ((btree-pathname (or btree-pathname (blank-identity))))      
    (if (probe-file btree-pathname)
      (planks.btree:find-btree
        btree-pathname)
      (apply #'planks.btree:make-btree
        (append (list btree-pathname :class 'persistent-heap-storage-btree) args)))))


(defclass persistent-heap-storage-btree (planks.btree:heap-btree)
  ()
  (:default-initargs
    :pathname   (blank-identity)
    :heap-size  (* 4 1024 1024)
    :key-type   'fixnum
    :key<       '<
    :key=       'eql
    :value-type '(vector (4))
    :value=     'equalp))


(defmethod print-object ((btree persistent-heap-storage-btree) stream)
  (with-slots (planks.btree::pathname planks.btree::heap-size planks.btree::heap-start
                planks.btree::free-space-start) btree
    (print-unreadable-object (btree stream :type t)
      (format stream "~A :heap-size ~D :heap-start ~D :free-start ~D"
        planks.btree::pathname planks.btree::heap-size planks.btree::heap-start
        planks.btree::free-space-start))))


;; (define-symbol-macro |heap| (ensure-heap "heap.btree"))


(defclass memory-mapped-red-black-tree (hh::memory-persistent-red-black-tree)
  ((pathname
     :initarg :pathname
     :accessor red-black-tree-pathname)
    (objects
      :initarg :objects
      :initform (make-array 0 :adjustable t :fill-pointer t)
      :accessor red-black-tree-objects
      :accessor objects)
    (forest
      :initarg :forest
      :accessor red-black-tree-forest
      :allocation :class)
    (stream
      :initarg :stream
      :accessor red-black-tree-stream)))


(defgeneric find-backing-heap-storage (persistent-storage-designator))

(defmethod  find-backing-heap-storage ((tree memory-mapped-red-black-tree))
  (ensure-heap (red-black-tree-pathname tree)))

(defmethod  find-backing-heap-storage ((heap persistent-heap-storage-btree))
  (ensure-heap (slot-value heap 'planks.btree::pathname)))


(defun make-memory-mapped-red-black-tree (&optional (pathname (blank-identity)))
  (let* (heap-storage-btree red-black-tree)
    (hh:with-rb-transaction
      ((setf red-black-tree     (make-instance 'memory-mapped-red-black-tree :pathname pathname)))
      (setf  heap-storage-btree (find-backing-heap-storage red-black-tree)))
    (values red-black-tree heap-storage-btree)))


(defmethod prb-open-storage ((tree memory-mapped-red-black-tree))
  (values tree (find-backing-heap-storage tree)))
  

(defmethod prb-stash-node ((tree memory-mapped-red-black-tree)
                            left-location right-location color-value key-value data-location)
  (let ((node-content
          (list left-location right-location color-value key-value data-location)))
    (values
      (vector-push-extend node-content (objects tree))
      (planks.btree:btree-insert (find-backing-heap-storage tree) key-value node-content))))



(defmethod prb-fetch-node ((tree memory-mapped-red-black-tree) location)
  (if (< location (length (objects tree)))
    (destructuring-bind
      (left right color key data) (aref (objects tree) location)
      (values left right color key data))
    (destructuring-bind
      (left right color key data) (planks.btree:btree-search
                                    (find-backing-heap-storage tree) location)
      (apply #'values
        (setf (aref (objects tree) location) (list left right color key data))))))


(defmethod prb-fetch-data ((tree memory-mapped-red-black-tree) location)
  (if (< location (length (objects tree)))
    (aref (objects tree) location)
    (setf (aref (objects tree) location)
       (planks.btree:btree-search
         (find-backing-heap-storage tree) location))))


(defmethod prb-stash-data ((tree memory-mapped-red-black-tree) data)
  (values 
    (vector-push-extend data (objects tree))
    (planks.btree:btree-insert
      (find-backing-heap-storage tree)
      (length (objects tree))
      data)))


(defmethod prb-close-storage ((tree memory-mapped-red-black-tree)))


(defmethod prb-location ((tree memory-mapped-red-black-tree))
  (length (objects tree)))


(defmethod prb-leaf-location-p ((tree memory-mapped-red-black-tree) location)
  (= 0 location))


(defmethod prb-save-root ((tree memory-mapped-red-black-tree) root)
  (setf (hh::state root) :unloaded)
  (setf (slot-value tree 'root) root))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; must mark as unloaded so that it behaves properly in next transaction

;; (btree-list)
;; (("path" . #<PERSISTENT-HEAP-STORAGE {100C36B593}>))

;; (defparameter *btree-file-root* (ensure-directories-exist #P"/tmp/pp-btree/"))

;; (defclass mmap-btree (file-btree) ())

;; (flet ((<> (arc-or-spread-path)
;;          nil)))


(planks:map-btree (planks:find-btree "rbtree") #'(lambda (k v) (format t "~a  => ~a~%" k v)))
