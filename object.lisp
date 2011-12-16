;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :fern)

(defclass uuid (unicly:unique-universal-identifier)())

(defmethod initialize-instance :after ((uuid uuid) &key)
  (or (get uuid) (put uuid)))

;; (make-instance 'uuid)
;;;; The class OBJECT
;;
;; object                           <CLASS>
;; object-identity-control-id       <SLOT>,<GENERIC>
;; object-identity-parent-uuid      <SLOT>,<GENERIC>
;; object-identity-uuid             <SLOT>,<GENERIC>
;; object-identity-uuid-byte-array  <SLOT>,<GENERIC>
;; object-identity-uuid-bit-vector  <SLOT>,<GENERIC>
;; object-identity-uuid-integer     <SLOT>,<GENERIC>
;; object-identity-uuid-string-36   <SLOT>,<GENERIC>
;; object-identity-uuid-version     <SLOT>,<GENERIC>
;;

(defclass object (uuid)
  ((object-identity-control-id
    :documentation  
    #.(format nil
              "An object identified by the `unicly:unique-universal-identifier'~%~
               per slot-value of object-identity-uuid."))
    
   (object-identity-parent-uuid
    :documentation  
    #.(format nil
              "An object of type `unicly:unique-universal-identifier' which acts~%~
              as the NAMESPACE arg to `unicly:make-v5-uuid' in conjunction with~%~
              slot-value of object-identity-control-id as the NAME arg."))
    
   (object-identity-uuid
    :documentation  
    #.(format nil
              "An object of type `unicly:unique-universal-identifier'.~%~@
               Value of this slot is suitable for use as a namespace argument to~%~@
               `unicly:make-v*-uuid'."))
    
   (object-identity-uuid-byte-array
    :documentation  
    #.(format nil
              "An object of type `unicly:uuid-byte-array-16'.~%~@
               Value of this slot is the byte-array representation of the object in slot~%~@
               object-identity-uuid."))
    
   (object-identity-uuid-bit-vector
    :documentation  
    #.(format nil
              "An object of type `unicly:uuid-byte-array-128'.~%~@
               Value of this slot is the bit-vector representation of the object in slot~%~@
               object-identity-uuid."))
    
   (object-identity-uuid-integer
    :documentation  
    #.(format nil
              "An object of type `unicly::uuid-integer-128'.~%~@
               Value of this slot is the 128bit integer representation of the object in slot~%~@
               object-identity-uuid."))
    
   (object-identity-uuid-string-36
    :documentation  
    #.(format nil
             "An object of type `unicly::uuid-hex-string-36'.~%~@
              Value of this slot is the hecadecimal integer representation of the object in slot~%~@
               object-identity-uuid."))
    
   (object-identity-uuid-version
    :documentation 
    #.(format nil 
              "The UUID version of the uuid namespace object in slot~%~@
               object-identity-uuid.")))
  
  (:documentation 
   #.(format nil
             "Instances of this class hold namespace metadata for classes whose instances~%~@
              share a common UUID namespace."))) 



;; (make-object :base-namespace (object-identity-uuid +statement+) :control-id "y")

    ;;       (slots (closer-mop:class-slots (find-class 'unique-universal-identifier)))
    ;;       (slot-names (mapcar #'c2mop:slot-definition-name slots)))
    ;; (dolist (s slot-names)
    ;;   (setf (slot-value o s) (slot-value uuid s)))))


#+()  
(defclass context (object)
   ())

(defmethod initialize-instance :after ((o object) &key (base-namespace  unicly:*UUID-NAMESPACE-URL*)
                                        (control-id (short-site-name)))
  (let ((new (make-v5-uuid (or base-namespace (object-identity-parent-uuid o))
    (or control-id (object-identity-control-id o)))))
    (describe new)
    (describe o)
   ;; :base-namespace (or base-namespace 
  ;;  :control-id (or control-id (object-identity-control-id o)))
  (:printv
    (let (;;(uuid (unicly:make-v5-uuid (or base-namespace unicly:*UUID-NAMESPACE-URL*)
         (slots (mapcar #'sb-mop:slot-definition-name
                  (sb-mop:class-slots (find-class 'unicly:unique-universal-identifier)))))
    (dolist (slot slots)
      (setf (slot-value o slot) (slot-value new slot)))
    (values o new)))))



;; (defun _::a (context node)
;;   (let* ((namestring (typecase node
;;                       (w:node (w:node-uri node))
;;                       (string node)))
;;           (node (make-instance '_::node :uri namestring)))
;;     (update-object node
;;       :base-namespace context :control-id namestring)))

;; (defmethod print-object ((o _::node) s)
;;   (print-object (w:node (w:node-uri o)) s))




;; mon:symbol-not-null-or-string-not-empty
;; mon::%fast-string-all-whitespace-p
(defun %verify-valid-string-or-symbol-for-identity (verify-identity)
  (declare #-:mon (type (or string (and symbol (not null))) verify-identity)
           #+:mon (type mon:symbol-not-null-or-string-not-empty verify-identity))
  #+:mon (unless (mon:symbol-not-null-or-string-not-empty-p verify-identity)
           (error "arg IDENTITY did not satisfy `mon:symbol-not-null-or-string-not-empty-p'"))
  #-:mon (when (and (stringp verify-identity)
                    (string= (make-string 0) verify-identity))
           (error "arg IDENTITY did not satisfy `mon:symbol-not-null-or-string-not-empty-p'"))
  (if (stringp verify-identity)
      (if #+:mon (mon::%fast-string-all-whitespace-p verify-identity)
          #-:mon (loop 
                    for char across verify-identity
                    always (member char (list #\SPACE #\NEWLINE #\TAB #\RETURN #\NO-BREAK_SPACE #\PAGE #\VT) :test 'char=))
          (error "arg IDENTITY must not be contained of all whitespace characters")
          verify-identity)
      verify-identity))

(defgeneric object-identity-parent-uuid (object)
  (:method  ((object object))
    (when (slot-boundp object 'object-identity-parent-uuid)
      (slot-value object 'object-identity-parent-uuid)))
  (:documentation "Return the base-namespace UUID for OBJECT'S object-identity-parent-uuid slot-value."))

(defgeneric (setf object-identity-parent-uuid) (uuid object)
  (:method  ((uuid unicly:unique-universal-identifier) (object object))
    (setf (slot-value object 'object-identity-parent-uuid)
          uuid))
  (:documentation "Set UUID as OBJECT's object-identity-parent-uuid slot-value."))

(defgeneric object-identity-uuid-byte-array (object)
  (:method ((object object))
    (when (slot-boundp object 'object-identity-uuid-byte-array)
      (slot-value  object 'object-identity-uuid-byte-array)))
  (:documentation "Accessor for OBJECT's object-identity-uuid-byte-array slot-value."))

(defgeneric (setf object-identity-uuid-byte-array) (byte-array object)
  (:method ((byte-array array) (object object))
    (declare (unicly::uuid-byte-array-16 byte-array))
    (setf (slot-value  object 'object-identity-uuid-byte-array)
          byte-array))
  (:documentation "Set OBJECT's UUID namespace byte-array representation with BYTE-ARRAY.
BYTE-ARRAY is an object of type `unicly:uuid-byte-array-16'."))

(defgeneric object-identity-uuid-bit-vector (object)
  (:method ((object object))
    (when (slot-boundp object 'object-identity-uuid-bit-vector)
      (slot-value object 'object-identity-uuid-bit-vector)))
 (:documentation "Accessor for OBJECTs uuid namespace bit-vector representation."))

(defgeneric (setf object-identity-uuid-bit-vector) (bv object)

  (:method  ((uuid-bit-vector bit-vector) (object object)) ; simple-bit-vector
    (declare (unicly::uuid-bit-vector-128 uuid-bit-vector))
    (setf (slot-value object 'object-identity-uuid-bit-vector)
          uuid-bit-vector))

  (:method :after
    ((uuid-bit-vector bit-vector) (object object)) ; simple-bit-vector
    (declare (unicly::uuid-bit-vector-128 uuid-bit-vector))
    (let ((slot-value-if (object-identity-uuid-version object))
          (version-if    (unicly::uuid-version-bit-vector uuid-bit-vector)))
      (declare ((mod 6) version-if))
      (when (zerop version-if)
        (error "Declining to set value for slot OBJECT-IDENTITY-UUID-VERSION ~
             to non-valid uuid version.~%~
             Likely the source UUID is corrupted or a null-uuid!~%~
             got bit-vector: ~S"
               uuid-bit-vector))
      (if (and slot-value-if (eql slot-value-if version-if))
          version-if
          (setf (object-identity-uuid-version object) version-if))))
  
  (:documentation "Set OBJECT's UUID namespace bit-vector representation with BV.
BV is an object of type `unicly:uuid-bit-vector-128'."))

(defgeneric object-identity-uuid-integer (object)
  (:method  ((object object))
    (when (slot-boundp object 'object-identity-uuid-integer)
      (slot-value object 'object-identity-uuid-integer)))
  (:documentation "Accessor for OBJECT's object-identity-uuid-integer slot-value."))

(defgeneric (setf object-identity-uuid-integer) (uuid-integer-128 object)
  (:method  ((uuid-integer-128 bignum) (object object))
    (declare (unicly::uuid-ub128 uuid-integer-128))
    (setf (slot-value object 'object-identity-uuid-integer)
          uuid-integer-128))
  (:documentation "Set OBJECT's 128bit integer representation with UUID-INTEGER-128."))

(defgeneric object-identity-uuid-string-36 (object)
  (:method  ((object object))
    (when (slot-boundp object 'object-identity-uuid-string-36)
    (slot-value object 'object-identity-uuid-string-36)))
  (:documentation "Accessor for OBJECT's object-identity-uuid-string-36 slot-value."))

(defgeneric (setf object-identity-uuid-string-36) (uuid-hex-string-36 object)
  (:method ((uuid-hex-string-36 string) (object object))
    (declare (unicly::uuid-hex-string-36 uuid-hex-string-36))
    (setf (slot-value object 'object-identity-uuid-string-36)
          uuid-hex-string-36))
  (:documentation "Set OBJECT's `unicly::uuid-hex-string-36' representation with UUID-HEX-STRING-36."))  

(defgeneric object-identity-uuid-version (object)
  (:method  ((object object))
    (when (slot-boundp object 'object-identity-uuid-version)
      (slot-value object 'object-identity-uuid-version)))
  (:documentation "Accessor for OBJECT's object-identity-uuid-version slot-value."))

(defgeneric object-identity-description (object &key stream verbose)
  (:documentation "Print slot-values of OBJECT to STREAM."))

(defgeneric (setf object-identity-uuid-version) (bv-or-string object)

  ;; (:method  ((integer integer) (object object))
  (:method  ((fixnum fixnum) (object object))
    (declare ((mod 6) fixnum))
    (when (zerop fixnum)
      (error "Declining to set value for slot OBJECT-IDENTITY-UUID-VERSION ~
            to non-valid uuid version. Likely the source UUID is corrupted!"))
    (setf (slot-value object 'object-identity-uuid-version)
          fixnum))

  (:method  ((uuid-bit-vector bit-vector) (object object))
    (declare (unicly::uuid-bit-vector-128 uuid-bit-vector))
    (let ((bv-version (unicly::uuid-version-bit-vector  uuid-bit-vector)))
      (declare ((mod 6) bv-version))
      (when (zerop bv-version)
        (error "Declining to set value for slot OBJECT-IDENTITY-UUID-VERSION~
            to non-valid uuid version. Likely the source UUID is corrupted!"))
      (setf (slot-value object 'object-identity-uuid-version)
            (unicly::uuid-version-bit-vector  uuid-bit-vector))))

  (:documentation "Set OBJECT's uuid version with BV-OR-STRING.
BV-OR-STRING is either an object of type `unicly:uuid-bit-vector-128' or an
integer in the range [1,5]"))

(defgeneric object-identity-control-id (object)
  (:method((object object))
    (when (slot-boundp object 'object-identity-control-id)
      (slot-value object 'object-identity-control-id)))
  (:documentation "Accessor for OBJECT's object-identity-control-id slot-value."))

;; (find-method #'(setf object-identity-control-id) '(:around) '(t object))
;; (remove-method #'(setf object-identity-control-id) (find-method #'(setf object-identity-control-id) '(:around) '(t object)))
(defgeneric (setf object-identity-control-id) (namespace-and-identity object)

  (:method  ((namespace-and-identity t) (object object))
    (setf (slot-value object 'object-identity-control-id)
          (cadr namespace-and-identity)))
  
  ;; :NOTE The NAMESPACE-AND-IDENTITY arg to the interface function
  ;; `make-object's contains the NAME arg for unicly:make-v5-uuid and
  ;; we want to store this as the slot-value of object-identity-control-id in order that we
  ;; may chase upwardly the class uuid's their namespaces and the parent
  ;; namespaces they descend from. To get the NAME into the slot-value of
  ;; object-identity-control-id we run an :around method which attempts to rollback in the
  ;; event of a failure (e.g. when the UUID representation for an arg ispoorly
  ;; configurued or otherwise illegitimate.
  (:method :around
    ((namespace-and-identity t) (object object))
    (declare (type list namespace-and-identity))
    (destructuring-bind (namespace identity) namespace-and-identity ;; (list (make-v4-uuid)  "<IDENTITY>")
      (declare ((and (or string symbol) (not null)) identity)
               (unicly:unique-universal-identifier namespace))
      (%verify-valid-string-or-symbol-for-identity identity)
      (let ((new-namespace       (unicly:make-v5-uuid namespace (if (symbolp identity) 
                                                                    (string identity)
                                                                    identity)))
            (old-id-slot         (object-identity-control-id                 object))
            (old-base-namespace  (object-identity-parent-uuid     object))
            ;; We might not have any slots set or only some, so get all of them.
            (old-namespace-slot  (object-identity-uuid            object))
            (old-byte-array-slot (object-identity-uuid-byte-array object))
            (old-bit-vector-slot (object-identity-uuid-bit-vector object))
            (old-integer-slot    (object-identity-uuid-integer    object))
            (old-hex-string-slot (object-identity-uuid-string-36  object))
            ;; (old-version-slot    (object-identity-uuid-version     object))
            (new-namespace-slot  '()))
        (unwind-protect 
             (progn 
               (setf new-namespace-slot 
                     (ignore-errors 
                       (setf (object-identity-uuid object)
                             new-namespace)))
               ;; If we didn't error we can safeley set the base-namespace slot
               ;; else unset what we just set...
               (when new-namespace-slot 
                 (setf (object-identity-parent-uuid object)
                       namespace)
                 (call-next-method)))
          (unless new-namespace-slot
            (let ((slot-val-for-rollback (or old-bit-vector-slot
                                             old-namespace-slot
                                             old-byte-array-slot
                                             old-integer-slot
                                             old-hex-string-slot)))
              ;; Following form will clear the existing object-identity-control-id and
              ;; object-identity-parent-uuid slots with slot-makunbound. 
              ;; We set them back to their previous state afterwards.
              (when slot-val-for-rollback
                (setf (object-identity-parent-uuid object)
                      slot-val-for-rollback))
              (if (and old-base-namespace old-id-slot)
                  (progn
                    (setf (object-identity-parent-uuid object) old-base-namespace)
                    ;; We set the slot-value explicitly instead of using the method
                    ;; specialized because it would land us right back here!
                    (setf (slot-value object 'object-identity) old-id-slot))
                  ;; If either the control-identity or base-namespace slots is
                  ;; null or unbound the the other should be as well.
                  (progn 
                    (slot-makunbound object 'object-identity-parent-uuid)
                    (slot-makunbound object 'object-identity-control-id)))))))))

  (:documentation "Set OBJECT's object-identity-control-id slot-value to IDENTITY"))

(defgeneric object-identity-uuid (object)
  (:method  ((object object))
    (when (slot-boundp object 'object-identity-uuid)
      (slot-value object 'object-identity-uuid)))
  (:documentation "Accessor for OBJECT's object-identity-uuid slot-value."))

(defgeneric (setf object-identity-uuid) (coercable-uuid object)

  ;; :NOTE `unicly:make-uuid-from-string' already coerces a uuid object with
  ;; `unicly:uuid-copy-uuid' we keep the method dispatch b/c we can check string
  ;; validity earlier.
  (:method  ((uuid-string string) (object object))
    (declare (unicly::uuid-hex-string-36 uuid-string))
    (let ((uuid-from-string (unicly:make-uuid-from-string uuid-string)))
      (declare (unicly:unique-universal-identifier uuid-from-string))
      (setf (slot-value object 'object-identity-uuid)
            uuid-from-string)))

  (:method  ((uuid unicly:unique-universal-identifier) (object object)) 
    (setf (slot-value object 'object-identity-uuid)
          (uuid-copy-uuid uuid)))

  (:method  ((uuid-bit-vector bit-vector) (object object))
    (declare (unicly::uuid-bit-vector-128 uuid-bit-vector))
    (let ((uuid-from-bv (unicly::uuid-from-bit-vector uuid-bit-vector)))
      (declare (unicly:unique-universal-identifier uuid-from-bv))
      (setf (slot-value object 'object-identity-uuid)
            uuid-from-bv)))

  (:method  ((uuid-byte-array array) (object object))
    (declare (unicly::uuid-byte-array-16 uuid-byte-array))
    (let ((uuid-from-byte-array (uuid-from-byte-array uuid-byte-array)))
      (declare (unicly:unique-universal-identifier uuid-from-byte-array))
      (setf (slot-value object 'object-identity-uuid)
            uuid-from-byte-array)))

  (:method  ((integer-128 bignum) (object object))
    (declare (unicly::uuid-ub128 integer-128))
    (let ((uuid-from-int (unicly::uuid-from-bit-vector 
                          (unicly::uuid-integer-128-to-bit-vector integer-128))))
      (declare (unicly:unique-universal-identifier uuid-from-int))
      (setf (slot-value object 'object-identity-uuid)
            uuid-from-int)))

  ;; The :after method helps us make sure all other slots get propagated whenever
  ;; a slot containing a uuid representation gets touched. 
  ;; This was an early 
  ;; (find-method #'(setf object-identity-uuid) '(:after) '(t object))
  (:method  :after 
    ((uuid-arg t) (object object))
    (let* ((uuid-bv-prevent (unicly:uuid-to-bit-vector 
                             (object-identity-uuid object)))
           (uuid-bv    (if (unicly:uuid-bit-vector-128-p uuid-arg)
                           ;; if the uuid-arg is the same bv128 as the conversion don't re-trigger
                           (if (unicly:uuid-bit-vector-eql uuid-arg uuid-bv-prevent)
                               nil
                               uuid-arg)
                           uuid-bv-prevent))
           (uuid-bytes (if (unicly:uuid-byte-array-16-p uuid-arg)
                           (let* ((existing-slot    
                                   (object-identity-uuid-byte-array object))
                                  (existing-slot-bv 
                                   (when existing-slot 
                                     (unicly:uuid-byte-array-to-bit-vector existing-slot)))
                                  (ba-to-bv 
                                   (if existing-slot-bv
                                       nil
                                       (unicly:uuid-byte-array-to-bit-vector uuid-arg))))
                             (if existing-slot-bv
                                 (if (unicly:uuid-bit-vector-eql existing-slot-bv uuid-bv-prevent)
                                     nil
                                     uuid-arg)
                                 (if (unicly:uuid-bit-vector-eql ba-to-bv uuid-bv-prevent)
                                     nil
                                     uuid-arg)))
                           (unicly:uuid-bit-vector-to-byte-array uuid-bv-prevent)))
           (uuid-int   (if (typep uuid-arg 'bignum)
                           (let* ((existing-slot    
                                   (object-identity-uuid-integer object))
                                  (existing-slot-bv
                                   (when existing-slot 
                                     (unicly::uuid-integer-128-to-bit-vector existing-slot)))
                                  (int-to-bv      
                                   (if existing-slot-bv
                                       nil
                                       (unicly::uuid-integer-128-to-bit-vector uuid-arg))))
                             (if existing-slot-bv
                                 (if (unicly:uuid-bit-vector-eql existing-slot-bv uuid-bv-prevent)
                                     nil
                                     uuid-arg)
                                 (if (unicly:uuid-bit-vector-eql int-to-bv uuid-bv-prevent)
                                     nil
                                     uuid-arg)))
                           (unicly::uuid-bit-vector-to-integer uuid-bv-prevent)))
           (uuid-hex-36   (if (and (stringp uuid-arg)
                                   (unicly::uuid-string-36-p (the string uuid-arg)))
                              (let* ((existing-slot
                                      (object-identity-uuid-string-36 object))
                                     (existing-slot-bv 
                                      (when existing-slot 
                                        (unicly::uuid-to-bit-vector (unicly:make-uuid-from-string existing-slot))))
                                     (hex-to-bv         
                                      (if existing-slot-bv
                                          nil
                                          (unicly::uuid-to-bit-vector (unicly:make-uuid-from-string uuid-arg)))))
                                (if existing-slot-bv
                                    (if (unicly:uuid-bit-vector-eql existing-slot-bv uuid-bv-prevent)
                                        nil
                                        uuid-arg)
                                    (if (unicly:uuid-bit-vector-eql hex-to-bv uuid-bv-prevent)
                                        nil
                                        uuid-arg)))
                              (unicly:uuid-princ-to-string (object-identity-uuid object)))))
      (declare (type (or unicly::uuid-byte-array-16 null)  uuid-bytes)
               (type (or unicly::uuid-bit-vector-128 null) uuid-bv)
               (type (or unicly::uuid-ub128 null)          uuid-int)
               (type (or unicly::uuid-hex-string-36 null)  uuid-hex-36))
      (when uuid-bv
        (setf (object-identity-uuid-bit-vector object) uuid-bv))
      (when uuid-bytes
        (setf (object-identity-uuid-byte-array object) uuid-bytes))
      (when uuid-hex-36
        (setf (object-identity-uuid-string-36  object) uuid-hex-36))
      (when uuid-int
        (setf (object-identity-uuid-integer    object) uuid-int))
      (if (object-identity-control-id object)
          (if (object-identity-parent-uuid object)
              ;; make sure that the namespace and identity evaluate to the
              ;; namespace we just set, and if not remove them.
              (unless (unicly::uuid-bit-vector-eql
                       (object-identity-uuid-bit-vector object)
                       (uuid-to-bit-vector
                        (make-v5-uuid
                         (object-identity-parent-uuid object)
                         (string (object-identity-control-id object)))))
                (slot-makunbound object 'object-identity-control-id)
                (slot-makunbound object 'object-identity-parent-uuid))
              (slot-makunbound object 'object-identity-control-id))
          ;; The control-identity isn't present, so if the base-namespace is as
          ;; well it shouldn't be
          (when (object-identity-parent-uuid object)
            (slot-makunbound object 'object-identity-parent-uuid)))
      uuid-arg))

  (:documentation "Set uuid namespace for OBJECT with COERCABLE-UUID
COERCABLE-UUID is a representation of a Unicly UUID in some form, e.g.:
 hex-string-36, byte-array-16, bit-vector-128, unique-universal-identifier"))

;; namespace-and-identity
(defun update-object (object &key base-namespace control-id)
  (declare (type object object)
           (type unicly:unique-universal-identifier base-namespace)
           #-:mon (type (or string (and symbol (not null))) control-id)
           #+:mon ((or mon:string-not-null-empty-or-all-whitespace mon:symbol-not-null)
                   control-id))
  ;; #-:mon (%verify-valid-string-or-symbol-for-identity identity)
  (let ((new-nmspc (unicly:make-v5-uuid base-namespace 
                                        (if (symbolp control-id) 
                                            (string control-id)
                                            control-id))))
    (setf (object-identity-control-id object) (list base-namespace control-id))
    (setf (object-identity-uuid object) new-nmspc))
  object)

(defun make-object (&key base-namespace control-id)
  (declare (unicly:unique-universal-identifier base-namespace)
           #-:mon (type (or string (and symbol (not null))) control-id)
           #+:mon ((or mon:string-not-null-empty-or-all-whitespace mon:symbol-not-null)
                   control-id))
  ;; #-:mon (%verify-valid-string-or-symbol-for-identity identity)
  (let ((new-obj   (make-instance 'object))
        (new-nmspc (unicly:make-v5-uuid base-namespace 
                                        (if (symbolp control-id) 
                                            (string control-id)
                                            control-id))))
    (setf (object-identity-control-id new-obj) (list base-namespace control-id))
    (setf (object-identity-uuid new-obj) new-nmspc)
    new-obj))


;; (find-method #'object-identity-description nil '(object))
(defmethod object-identity-description ((object object) &key stream verbose)
  (declare (type boolean verbose))
  (if (not verbose)
      (let* ((unbound "#<UNBOUND>")
             (id-if  (object-identity-control-id object))
             (id     (if id-if
                             (prin1-to-string id-if)
                             unbound))
             (id-uuid   (or (object-identity-uuid object)
                                unbound)))
        (with-standard-io-syntax 
          (format stream "~%~{~4T~29A~A~^~%~}~4T"
                  (list 
                   ;; "TYPE-OF:"    (type-of object)
                   ":OBJECT-IDENTITY-CONTROL-ID"          id
                   ":OBJECT-IDENTITY-UUID"     id-uuid))))

      (let* ((unbound "#<UNBOUND>")
             (id-if  (object-identity-control-id object))
             (id     (if id-if
                             (prin1-to-string id-if)
                             unbound))
             (id-uuid   (or (object-identity-uuid object)
                                unbound))
             (byte-array    (or (object-identity-uuid-byte-array object)
                                unbound))
             (bit-vector    (or (object-identity-uuid-bit-vector object)
                                unbound))
             (hex-string-if (object-identity-uuid-string-36 object))
             (hex-string    (if hex-string-if
                                (prin1-to-string hex-string-if)
                                unbound))
             (integer-128-if (object-identity-uuid-integer object))
             (integer-128    (if integer-128-if
                                 (let ((*print-base* 16)
                                       (*print-radix* t)) 
                                   (princ-to-string integer-128-if))
                                 unbound))
             (parent-uuid    (or (object-identity-parent-uuid object)
                                 unbound))         
             (version-if    (object-identity-uuid-version object))
             (version       (if version-if 
                                (prin1-to-string version-if)
                                unbound))
             (format-description
              (let ((*print-lines* 0))
                (format nil "~%~{~4T~40A~A~%~}"
                        (list ;; "TYPE-OF:"    (type-of object)
                         ":OBJECT-IDENTITY-CONTROL-ID"                  id
                         ":OBJECT-IDENTITY-UUID"             id-uuid
                         ":OBJECT-IDENTITY-PARENT-UUID"      parent-uuid
                         ":OBJECT-IDENTITY-UUID-STRING-36"   hex-string
                         ":OBJECT-IDENTITY-UUID-BYTE-ARRAY"  byte-array
                         ":OBJECT-IDENTITY-UUID-INTEGER"     integer-128
                         ":OBJECT-IDENTITY-UUID-BIT-VECTOR"  bit-vector
                         ":OBJECT-IDENTITY-UUID-VERSION"     version)))))
        (with-standard-io-syntax 
          (princ format-description stream)))))

(defmethod describe-object ((object object) stream)
  (print (type-of object) stream)
  (object-identity-description object :stream stream :verbose t))

(defmethod print-object ((object object) stream)
  (print-unreadable-object (object stream :type t) ;; :identity t)
    (object-identity-description object :stream  stream)))

#+()
(defclass context (object)
  ((state :initarg :state :accessor context-state))
  (:default-initargs :state (make-trie)))


;;; ==============================
;;
#|
(define-symbol-macro bubba#
  (defparameter _::bubba#
    (make-object :base-namespace unicly:*uuid-namespace-url*
      :control-id "bubba")))
*TT--OBJ-UUID*
|#
;; => *TT--OBJ-UUID*
;;
;; *TT--OBJ-UUID*
;; |=> #<OBJECT 
;; |     ID:      "bubba"
;; |     UUID:    eea1105e-3681-5117-99b6-7b2b5fe1f3c7>
;;
;; (unicly:uuid-eql (make-v5-uuid (object-identity-control-id-parent-uuid  *tt--obj-uuid*)
;;                                (object-identity-control-id  *tt--obj-uuid*))
;;                  (object-identity-control-id-uuid *tt--obj-uuid*))
;; => T
;;
;; (setf *tt--obj-uuid*
;;       (make-object :base-namespace unicly:*uuid-namespace-dns* 
;;                                :control-id 'bubba))
;; |=> #<OBJECT 
;; |     ID:      BUBBA
;; |     UUID:    60ada823-d6de-5729-9e7e-8e44a57e400d >
;;
;; (unicly:uuid-eql (make-v5-uuid (object-identity-control-id-parent-uuid  *tt--obj-uuid*)
;;                                 (string (object-identity-control-id  *tt--obj-uuid*)))
;;                   (object-identity-control-id-uuid *tt--obj-uuid*))
;; => T
;;
;; (setf (object-identity-control-id *tt--obj-uuid*)
;;       (list (make-v4-uuid) "new-bubba"))
;; => "new-bubba"
;;
;; *tt--obj-uuid*
;;  |=> #<OBJECT 
;;  |    :OBJECT-IDENTITY-CONTROL-ID             "new-bubba"
;;  |    :OBJECT-IDENTITY-CONTROL-ID-UUID        0ef86c6f-b263-5987-a90c-3fcca581bc38 >
;;
;; (object-identity-control-id-uuid  *tt--obj-uuid*)
;; ; => 0ef86c6f-b263-5987-a90c-3fcca581bc38
;;
;; (object-identity-control-id  *tt--obj-uuid*)
;;  ;=> "new-bubba"
;;
;; (object-identity-control-id-parent-uuid  *tt--obj-uuid*)
;;  ;=> addedf68-81a4-47cd-a7b1-b96779f8b676
;; 
;; (type-of (object-identity-control-id-parent-uuid  *tt--obj-uuid*))
;; => UNIQUE-UNIVERSAL-IDENTIFIER
;;
;; (unicly:uuid-version-uuid (object-identity-control-id-parent-uuid  *tt--obj-uuid*))
;; ;=> 4
;;
;; (object-identity-control-id-uuid-version *tt--obj-uuid*)
;; ;=> 5
;;
;; (object-identity-control-id-uuid-byte-array  *tt--obj-uuid*)
;; ;=> #(14 248 108 111 178 99 89 135 169 12 63 204 165 129 188 56)
;;
;; (object-identity-control-id-uuid-bit-vector  *tt--obj-uuid*)
;; ;=> #*000011101111100001101100011011111011001001100011010 ...
;;
;; (object-identity-control-id-uuid-integer     *tt--obj-uuid*)
;; ;=> 19899080911677131133725998230922181688
;;
;; (object-identity-uuid-string-36   *tt--obj-uuid*)
;; ;=> "0ef86c6f-b263-5987-a90c-3fcca581bc38"
;;
;; (update-object *tt--obj-uuid*
;;                            :base-namespace (make-v5-uuid *uuid-namespace-dns* (string '*tt--obj-uuid*))
;;                            :control-id '*tt--obj-uuid*)
;; |=> #<OBJECT 
;; |    :OBJECT-IDENTITY-CONTROL-ID             *TT--OBJ-UUID*
;; |    :OBJECT-IDENTITY-UUID        db197774-6955-55b1-ac30-143864977f41 >
;;
;;
;; (object-identity-control-id *TT--OBJ-UUID*)
;; ;=> *TT--OBJ-UUID*
;;
;; (eq (object-identity-control-id *TT--OBJ-UUID*) '*TT--OBJ-UUID*)
;; ;=> T
;;
;;; ==============================


;; *package*
