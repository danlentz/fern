;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;
;;;;; OKBC-TRIES: efficient, (consp), key-value storage
;;;;;

(defpackage #:fern
  (:use    #:common-lisp)
  (:shadow #:copy #:delete #:map #:count #:get #:dismantle)
  (:export #:make #:copy #:delete #:get #:put #:get-node #:get-node* #:map #:map-nodes
           #:map-values #:filter #:count #:all-values
           #:make-trie #:make-fern #:delete-trie #:get-trie #:get-trie-returning-node
           #:fern-name #:fern-id #:fern-plist #:fern-trie
           #:*list-to-hashtable-threshold* #:*initial-hash-table-size* #:*rehash-size*))

(in-package :fern)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Concept
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; There are three easy ways to linearize lists: (p a (b . c))
;;
;; 1. (cons p cons a cons cons b cons c cons nil)
;; 2. (start p a start b dot c dot nil)
;; 3. (start p a start b dot c end)
;;
;; The original trie code in Norvig uses (1).  Both (2) and (3) are more
;; compact, with (3) being the most compact.  Because the extra code
;; to handle (3) is truly minimal, and it is general and (always) more
;; compact, we use it instead.
;;
;; Stashing.  We do not linearize the entire list, but only as much as we need to.
;; The rest is stashed. The above list might be linearized as:
;;
;; 4a. (start (p a (b . c)))
;; 4b. (start p a ((b . c)))
;; 4c. (start p a start (b . c))
;;
;; depending on the contents.  The 'stash' holds the rest of a list,
;; which already has a 'start' node.
;;
;; This is *much* more compact for the test cases that I've looked at
;; so far.  Typical improvements are a factor of 10-20 in storage
;; usage.  E.g. from 18mb to 0.45mb.  This has a significant impact on
;; GC behavior.
;;
;; each trie is single cons cell
;;
;;  (value . arcs), or
;;  (value . (stash . (stashed-list . trie)))
;;
;; Automatic representaion.
;;
;; Promotes trie nodes to hash tables when the number of arcs exceeds
;; `*list-to-hashtable-threshold*`
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Identifiers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These tries work nicely with UUIDs so we include a couple of conveniences

(defun uuid-print-urn (uuid &optional (stream nil))
  (format stream "urn:uuid:~A" uuid))

(defun uuid-urn-string-p (string)
  (and (stringp string)
    (eql 45 (length string))
    (string-equal (subseq string 0 9) "urn:uuid:")))

(defun uuid-parse-urn (string)
  (if (uuid-urn-string-p string)
    (uuid:make-uuid-from-string (subseq string 9))
    (error " ~A does not conform to:~% urn:uuid:XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX" string)))

(defparameter /uri-pattern-string/ "^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?"
  "The regular-expression from rfc3986. NB. the escaped '?'.")

(defparameter +null+ (uuid:make-null-uuid))
(defparameter +url+  uuid:+namespace-url+)
(defparameter +oid+  uuid:+namespace-oid+)
(defparameter +dns+  uuid:+namespace-dns+)
(defparameter +x500+ uuid:+namespace-x500+)
(defparameter +t+    (uuid:make-v5-uuid +url+ (uiop:hostname)))

(defun v1 () (uuid:make-v1-uuid))
(defun v4 () (uuid:make-v4-uuid))
(defun v5 (thing &optional (context +t+))
  (uuid:make-v5-uuid (or context +null+) thing))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +start+    '+START+
  "Special value to indicate the start of a list.")

(defconstant +end+      '+END+
  "Special value to indicate the end of a list.")

(defconstant +dot+      '+DOT+
  "Special value to indicate a dotted pair.")

(defconstant +stash+    '+STASH+
  "Special value to indicate a stashed list.")

(defconstant +no-value+ '+NO-VALUE+
  "Special value to indicate that the value cell of a `trie` empty.")

(defparameter *list-to-hashtable-threshold*  20)
(defparameter *initial-hash-table-size*     500)
(defparameter *rehash-size*                 3.0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Util
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric fast-hash-key (thing)
  (:documentation "In some Lisp implementations, some data structures hash
   much faster or more evenly than others.  For example, symbols might have
   better hashing behavior than CLOS instances.  Because of this difference
   in performance, a number of places in the OKBC implementation indirect
   before hashing occurs from objects to the hash keys that give faster hashing
   for those objects.

   This generic function maps from an object to the fast-hash-key for that
   object.  If there is a fast-hash-key for an object, it will typically be
   a symbol in a slot on that object, and a method will be defined on this
   generic function that is a slot accessor for that object.  In some
   implementations, the difference in hashing performance can be so great
   as more than to outweigh the method dispatch to get the fast-hash-key."))

(defmethod fast-hash-key ((thing t)) thing)

(defun car-equal (x y)
  "The car of X is equal to the car of Y."
  (cond
    ((eql x y) t)
    ((consp x) (and (consp y)
	            (equal (car (the cons x))
                           (car (the cons y)))))
    (t nil)))

(defmacro alias (to from)
  `(prog1 ',to
     (setf (fdefinition (intern (symbol-name ',to)))
           (fdefinition ',from))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Root
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *root* nil)

(defun default-root-name ()
  (cl:values '_ (v5 (symbol-name '_))))

(defvar *serial* (cons nil 0))

(defstruct (fern (:constructor %make-fern)
                 (:copier %copy-fern))
  "Structure for the root node of a `trie` discrimination net.  This has no semantic difference
   from normal `trie` nodes, but provides slots that allow for a backpointer to the parent
   context, a unique identifier, and potentially a name and property list metadata."
  trie name plist id up)

(defun make-trie ()
  "Makes a new empty `trie`."
  (cons +no-value+ nil))

(declaim (inline deref))
(defun deref (trie-designator)
  (if (fern-p trie-designator) (fern-trie trie-designator) trie-designator))

(defun make-fern (&optional name (up (root)) plist)
  "Creates a new, empty `root-fern`, initializing the `name` and context plist metadata"
  (let* ((serial (sb-ext:atomic-incf (cdr *serial*)))
         (name   (or name (gensym serial))))
    (%make-fern :name name :up up :plist `(:serial ,serial ,@plist)
                :id (v5 (princ-to-string name) (or (fern-id up) +t+)) :trie (make-trie))))

(alias make make-fern)

(defun root ()
  (or *root* (multiple-value-bind (name id) (default-root-name)
               (setf *root* (%make-fern :name name :id id :trie (make-trie)
                                        :plist `(:serial ,(sb-ext:atomic-incf (cdr *serial*))))))))
(defun flush-root! ()
  (setf *root* nil) (root))

(define-symbol-macro <> (root))
(assert (root))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Trie Primitives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun trie-value (trie &aux (trie (deref trie)))
  "Returns the value located at a `trie` node."
  (declare (cons trie))
  (car trie))

(defun value (trie &aux (trie (deref trie)))
  (trie-value trie))

(defun set-trie-value (trie value &aux (trie (deref trie)))
  "Sets the value slot at a given `trie` node."
  (setf (trie-value trie) value))

(defun (setf trie-value) (value trie)
  (declare (cons trie))
  (setf (car trie) value))

(defun (setf value) (value trie)
  (setf (trie-value (deref trie)) value))

(defun set-value (trie value)
  (setf (trie-value (deref trie)) value))

(defun trie-arcs (trie)
  "Returns the list of arcs dangling from a given trie node."
  (declare (cons trie))
  (let ((x (cdr trie)))
    (if (and (consp x) (eq (car x) +stash+))
	nil x)))

(defun (setf trie-arcs) (value trie)
  (declare (cons trie))
  ;; (assert (not (member +stash+ trie)))
  (setf (cdr trie) value))

(defun trie-stash (trie)
  (declare (cons trie))
  (let ((x (cdr trie)))
    (if (and (consp x) (eq (car x) +stash+))
	(cdr x) nil)))

(defun (setf trie-stash) (value trie)
  (declare (cons trie))
  (if (null value)
      (let ((x (cdr trie)))
	(when (eq (car x) +stash+)
	  (setf (cdr trie) nil)))
      (setf (cdr trie) (cons +stash+ value)))
  value)

(defun share-one-arc (l1 l2)
  (or (eql l1 l2) (and (consp l1) (consp l2))))

(defun trie-arcs-hashed-p (trie)
  "A predicate that is trie of a `trie` node if the node has had
   enough arcs depending on it to push it into hashed mode."
  (not (listp (trie-arcs trie))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expression Linearization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; linearize-sexpr and delinearize-sexpr use the same linearization as
;; the trie, except that linearize-sexpr linearizes the complete
;; expression, rather than doing it lazily.  The delinearize-sexpr
;; will, however, handle lazy linearization.

(defun linearize-sexpr (x)
  "Linearizes an sexpression `x`.  This is the same representation
   used in the trie indexing structure.  For example, the linearized version of
   the sexpr `(a (b) c . d)` is
   `(*start* a *start* b *end* c *dot* d)`.
   Also see `delinearize-sexpr`."
  (if (consp x)
      (cons +start+ (linearize-rest x))
      (list x)))

(defun linearize-rest (x)
  (cond
    ((null x) (list +end+))
    ((atom x) (list +dot+ x))
    (t (append
	(linearize-sexpr (car x))
	(linearize-rest (cdr x))))))

(defun delinearize-sexpr (x)
  "`X` is a linearized sexpression.  Return the sexpression that it
   represents.  See `linearize-sexpr`.  Handles lazy linearization in
   the form of non-atomic elements.  E.g., `(*start* a (b c))` is
   `(a b c)`.  An error is signalled if `X` does not represent a complete
   sexpression."
  (multiple-value-bind (key rest)
      (delinearize-sexpr-1 x)
    (assert (null rest))
    key))

(defun delinearize-sexpr-1 (x)
  (cond
    ((atom x)                (values x nil))
    ((eq (first x) +start+)  (delinearize-sexpr-rest (cdr x)))
    (t                       (values (car x) (cdr x)))))

(defun delinearize-sexpr-rest (x)
  (cond ((atom x) (values nil nil))
	((eq (car x) +end+) (values nil (cdr x)))
	((eq (car x) +start+)
	 (multiple-value-bind (partial-key rest)
	     (delinearize-sexpr-1 x)
	   (multiple-value-bind (tail new-rest)
	       (delinearize-sexpr-rest rest)
	     (values (cons partial-key tail) new-rest))))
	((eq (car x) +dot+)
	 (multiple-value-bind (partial-key rest)
	     (delinearize-sexpr-1 (cdr x))
	   (values partial-key rest)))
        ((consp (car x)) ;stashed value
         (values (car x) (cdr x)))
	(t (multiple-value-bind (partial-key rest)
	     (delinearize-sexpr-rest (cdr x))
	     (values (cons (car x) partial-key)
		     rest)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun delete-trie (key trie &aux (trie (deref trie)))
  "Marks the entry for `key` in the `trie` as deleted. This does *not* remove
   the key from the trie. Returns T if key was in the trie, else nil."
  (let ((key-trie (find-trie key nil nil trie)))
    (when key-trie (set-trie-value key-trie +no-value+) t)))

(defun delete (trie key)
  (delete-trie key trie))

(defun get-trie (key trie &aux (trie (deref trie)))
  "Returns the value for a `key` in a `trie`, and t/nil if found.  This function
   is equivalent to `gethash`.  The second value returned is true if an entry was found."
  (let* ((key-trie (find-trie key nil nil trie))
	 (val (when key-trie (trie-value key-trie))))
    (if (or (null key-trie) (eq val +no-value+))
	(values nil nil)
        (values val t))))

(defun get (trie key)
  (get-trie key trie))

(defun get-trie-returning-node (key trie &optional (default nil))
  "Returns the value for a `key` in a `trie`, and t/nil
   if found.  The second value returned is true if an entry was found, and
   false otherwise.  The third value is the trie node locating the key in the
   trie's net, so that we can do a modify on the locative without having to
   do the get again.  This function has uses analogous to the old modify-hash
   of Zetalisp."
  (if (fern-p trie) (get-trie-returning-node key (fern-trie trie) default)
      (let* ((key-trie (find-trie key t nil trie))
	     (val (when key-trie (trie-value key-trie))))
        (if (or (null key-trie) (eq val +no-value+))
	    (values default nil key-trie)
	    (values val t key-trie)))))

(defun get-trie-returning-node-no-extend (key trie &optional (default nil))
  "Returns the value for a `key` in a `trie`, and t/nil
   if found.  The second value returned is true if an entry was found, and
   false otherwise.  The third value is the trie node so that we can do a
   modify on the locative without having to do the get again.  If the node is
   not found, we do NOT extend, so no the third returned value is not
   defined.  See also `get-trie-returning-node`."
  (if (fern-p trie) (get-trie-returning-node-no-extend key (fern-trie trie) default)
      (let* ((key-trie (find-trie key nil nil trie))
	     (val (when key-trie (trie-value key-trie))))
        (if (or (null key-trie) (eq val +no-value+))
	    (values default nil key-trie)
	    (values val t key-trie)))))

(defun (setf get-trie) (value key trie)
  "Set the value of key in trie."
  (set-trie-value (find-trie key t nil trie) value))

(defun (setf get) (value key trie)
  "Set the value of key in trie."
  (set-trie-value (find-trie key t nil trie) value))

(defun put (trie key &optional (value key) &aux (trie (deref trie)))
  (set-trie-value (find-trie key t nil trie) value))

;(put <> (gensym))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-trie-arc (key trie &aux (trie (deref trie)))
  (if (trie-arcs-hashed-p trie)
      (let ((hash-key (fast-hash-key key)))
	(gethash hash-key (the hash-table (trie-arcs trie))))
      ;; cdr is a trie, can't be nil.
      (if (stringp key)
	  (loop for entry in (the list (trie-arcs trie))
		for arc-key = (car (the cons entry))
		when (and (stringp arc-key)
			  (string= (the simple-string key)
				   (the simple-string arc-key)))
		return (cdr entry))
	  (loop for entry
		in (the list (trie-arcs trie))
		when (eql key (first (the cons entry)))
		return (cdr entry)))))

(defun trie-could-follow-one-arc (key trie)
  (get-trie-arc (if (atom key) key +start+) trie))

(defun trie-follow-arc (component extend? trie &aux (trie (deref trie)))
  "Find the trie node for this component of the key. If EXTEND? is true, make a new node when necessary."
  (let ((next-trie (get-trie-arc component trie)))
    (or next-trie
	(if (not extend?)
	    nil
	    (let ((new-trie (make-trie)))
	      (cond
		((trie-arcs-hashed-p trie) (setf (gethash (fast-hash-key component) (trie-arcs trie)) new-trie))
		((>= (length (the list (trie-arcs trie))) *list-to-hashtable-threshold*)
		 ;; List is too long, convert to a hashtable ;; (princ "H")
		 (let ((ht (make-hash-table
			    :test #'equal
			    :rehash-size *rehash-size*
			    :size *initial-hash-table-size*)))
		   (loop for entry in (trie-arcs trie)
			 do (setf (gethash (fast-hash-key (car entry)) ht)
			       (cdr entry)))
		   (setf (gethash (fast-hash-key component) ht) new-trie)
		   (setf (trie-arcs trie) ht)))
		(t  (push (cons component new-trie) (trie-arcs trie))))
	      new-trie)))))

(defun find-trie (key extend? path trie &aux (trie (deref trie)))
  "Find the trie node for this key. If EXTEND?, make a new node if necessary."
  (when (not (null trie))
    (if (consp key)
	(find-trie-list key extend? path (find-trie +start+ extend? nil trie))
	(trie-follow-arc key extend? trie))))

(defun find-trie-list-no-stash (list extend? path trie)
  (cond
    ((null list)
     (find-trie +end+ extend? path trie))
    ((atom list)
     (find-trie list extend? path (find-trie +dot+ extend? nil trie)))
    (t
     (find-trie-list
      (cdr list) extend?
      (if (car-equal list path) (cdr path) nil)
      (find-trie (car list) extend? (and (consp path) (car path)) trie)))))

(defun find-trie-list (list extend? path trie &aux (trie (deref trie)))
  (if (null trie)
      (if extend? (error "shouldn't be here") nil)
      (let ((stash (trie-stash trie)))
	(if stash
	    (if (and list (equal list (car stash)))
		(cdr stash)
		(if (not extend?)
		    (find-trie-list-no-stash list nil nil trie)
		    (progn
		      (setf (trie-stash trie) nil)
		      (let ((new-for-stash
			     (find-trie-list
			      (cdr (car stash)) t
			      (if (car-equal list (car stash)) (cdr list) nil)
			      (find-trie (car (car stash))
					 'FORCE (and (consp list) (car list))
					 trie))))
			(set-trie-value new-for-stash (trie-value (cdr stash)))
			;; Set arc or stash.  We guarantee that only one is there
			(if (trie-arcs (cdr stash))
			    (setf (trie-arcs new-for-stash)
				  (trie-arcs (cdr stash)))
			    (setf (trie-stash new-for-stash)
				  (trie-stash (cdr stash))))
			;; now try to index list again.  trie no longer has a stash.
			(find-trie-list-no-stash list t path trie)))))
	    (if (not extend?)
		(find-trie-list-no-stash list nil nil trie)
		(if (or (eql extend? 'FORCE)
			(not (consp list)) ; don't stash atoms!
			(trie-arcs trie)
			(share-one-arc	; don't stash if list matchs path
			 (car (the cons list)) (and (consp path) (car path))))
		    ;; don't want to stash here
		    (find-trie-list-no-stash list t path trie)
		    ;; do want to stash here
		    (cdr (setf (trie-stash trie) (cons list (make-trie))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mapping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun maptrie (f trie &optional (parent nil supplied-p) &aux (trie (deref trie)))
  "Calls the function `f` on each key/value pair stored in the
   `trie`.  Note that this is fairly expensive -- the key must be
   delinearized.  If you don't really need to use the key for the values in
   the trie, you should use `maptrie-values`."
  (let* ((value (trie-value trie)))
    (when (and supplied-p (not (eql value +no-value+)))
      (funcall f (delinearize-sexpr (reverse parent)) value))
    (cond
      ((trie-stash trie)          (maptrie f (cdr (trie-stash trie))
                                        (cons (car (trie-stash trie)) parent)))
      ((trie-arcs-hashed-p trie)  (maphash #'(lambda (key node)
                                               (maptrie f node (cons key parent)))
	                                   (trie-arcs trie)))
      (t (let ((arcs (trie-arcs trie)))
	   (when arcs
	     (if (null (cdr arcs))
	         (maptrie f (cdr (car arcs)) (cons (car (car arcs)) parent)) ;; tail recurse on 1 arc
	         (loop for (key . node) in (trie-arcs trie)
		       do (maptrie f node (cons key parent))))))))))

(alias map maptrie)

(defun maptrie-nodes (f trie &optional (with-values t) &aux (trie (deref trie)))
  "Calls the function `f` on each `trie` node.  If `with-values` is true, the default,
    then only calls `f` on nodes with a value."
    (when (or (null with-values) (not (eql (trie-value trie) +no-value+)))
      (funcall f trie))
    (cond
      ((trie-arcs-hashed-p trie)
       (maphash #'(lambda (key node)
		    (declare (ignore key))
		    (maptrie-nodes f node))
	        (trie-arcs trie)))
      ((trie-stash trie)
       (maptrie-nodes f (cdr (trie-stash trie))) with-values)
      (t
       (let ((arcs (trie-arcs trie)))
         ;; If there is just one arc, then we can be tail-recursive
         (when arcs
	   (if (cdr (trie-arcs trie))
	       (loop for pair in arcs
		     do (maptrie-nodes f (cdr pair) with-values))
	       (maptrie-nodes f (cdr (car arcs)) with-values)))))))

(alias map-nodes maptrie-nodes)

(defun dismantle-trie (trie &aux (trie (deref trie)))
  "Destructively dismantles the `trie` and all of its inferiors."
  (set-trie-value trie +no-value+)
  (cond
    ((trie-arcs-hashed-p trie)
     (maphash #'(lambda (key node)
		  (declare (ignore key))
		  (dismantle-trie node))
	      (trie-arcs trie))
     (clrhash (trie-arcs trie)))
    ((trie-stash trie)
     (dismantle-trie (cdr (trie-stash trie)))
     (setf (trie-stash trie) nil))
    (t
     (let ((arcs (trie-arcs trie)))
       ;; If there is just one arc, then we could be tail-recursive
       (loop for pair in arcs
	     do (dismantle-trie (cdr pair)))
       (setf (trie-arcs trie) nil)))))

(alias dismantle dismantle-trie)

(defun maptrie-nodes-remove-if (f trie &optional (key nil) &aux (trie (deref trie)))
  "Maps over a `trie` conditionally removing nodes.  The nodes in
   the `trie` are visited top-down.  The function `f` is
   applied to three arguments: a `trie`, a key, and a value.  The
   key is the label on the arc leading to this `trie`.  The key will
   be nil for the top level trie.  If `f` returns true, then the
   subtree under the `trie` is dismantled and the arc
   leading to it is removed."
  (cond
    ((and key
	  ;;(not (eq trie-deleted value)) ;; snip out even if deleted
	  (funcall f trie key (trie-value trie)))
     ;; passed the test, dismantle the inferiors and return T
     (dismantle-trie trie) T)
    (t
     ;; this trie is ok, but recurse.
     (cond
       ((trie-arcs-hashed-p trie)
	(maphash #'(lambda (arc-key arc-trie)
		     (when (maptrie-nodes-remove-if
			    f arc-trie arc-key)
		       (remhash arc-key (trie-arcs trie))))
		 (trie-arcs trie)))
       ((trie-stash trie)
	(when (maptrie-nodes-remove-if
	       f (cdr (trie-stash trie)) (car (trie-stash trie)))
	  (setf (trie-stash trie) nil)))
       (t
	(loop for arc in (trie-arcs trie)
	      for result = (maptrie-nodes-remove-if
			    f (rest arc) (first arc))
	      when result
	      do (setf (trie-arcs trie)
		       (cl:delete arc (trie-arcs trie) :count 1)))))
     ;; and return nil
     nil)))

(defun maptrie-values (f trie &aux (trie (deref trie)))
  "Calls the function `f` on each value stored in the `trie`."
  (maptrie-nodes #'(lambda (trie) (funcall f (trie-value trie))) trie))

(alias map-values maptrie-values)

(defun trie-values (trie)
  "Returns a list of all of the values in the `trie`."
  (let (acc)
    (maptrie-values (lambda (x) (push x acc)) trie)
    acc))

(alias vals trie-values)

(defun maptrie-keys (f trie &aux (trie (deref trie)))
  "Calls the function `f` on each value stored in the `trie`."
  (maptrie (lambda (k v) (declare (ignore v)) (funcall f k)) trie))

(alias map-keys maptrie-keys)

(defun trie-keys (trie)
  "Returns a list of all of the keys in the `trie`  Potentially expensive."
  (let (acc)
    (maptrie-keys (lambda (x) (push x acc)) trie)
    acc))

(alias keys trie-keys)

(defun trie-node-count (trie &optional (with-values t) &aux (trie (deref trie)))
  "Returns the number of nodes in the `trie` that have values."
  (let ((max 0) (count 0))
    (maptrie-nodes
     #'(lambda (trie)
	 (let ((len (if (trie-arcs-hashed-p trie)
			(hash-table-count (trie-arcs trie))
			(length (trie-arcs trie)))))
	   (incf count)
	   (setq max (max max len))))
     trie with-values)
    (values count max)))

(alias count trie-node-count)

(defun copy-deep-trie (trie &aux (trie (deref trie)))
  "Given a `trie` node makes a deep copy of it, copying all nodes
   beneath it, but not copying the values in the nodes themselves."
  (let ((new (make-trie)))
    (set-trie-value new (trie-value trie))
    (if (trie-stash trie)
	(setf (trie-stash new)
	      (cons
	       ;;(copy-tree (car (trie-stash trie)))
	       ;; Don't copy-tree the list structure of the trie stash.  The
	       ;; Goal is to make a new trie with new indexing structure, but
	       ;; we'd still like to structure-share with any non-disassembled
	       ;; list structure.
	       (car (trie-stash trie))
	       (copy-deep-trie (cdr (trie-stash trie)))))
	(let ((arcs (trie-arcs trie)))
	  (setf (trie-arcs new)
		(cond
		  ((trie-arcs-hashed-p trie)
		   (let ((ht (make-hash-table
			      :test (hash-table-test arcs)
			      :rehash-size *rehash-size*
			      :size
			      (ceiling (* 1.6 (hash-table-count arcs))))))
		     (maphash
		      #'(lambda (key value)
			  (setf (gethash key ht) (copy-deep-trie value)))
		      arcs)
		     ht))
		  (t
		   (loop for (key . value) in arcs
			 collect (cons key (copy-deep-trie value))))))))
    new))

(alias copy copy-deep-trie)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Print
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((fern fern) stream)
  (print-unreadable-object (fern stream :type t :identity nil)
    (format stream "~A:~A [~d/~d]"
            (or (if (fern-up fern) (fern-name (fern-up fern))) :_)
            (or (fern-name fern) (format nil "ID~D" (fern-id fern)))
            (trie-node-count (fern-trie fern))
            (trie-node-count (fern-trie fern) t))))  ;; todo: fix this
