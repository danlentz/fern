;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;
;;;;; OKBC-TRIES: efficient, (consp), key-value storage

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Identifiers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
;; Trie Primitives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *id* (cons nil 0))

(defstruct (fern (:constructor %make-fern)
                 (:copier %copy-fern))
  "Structure for the root node of a `trie` discrimination net.  This has no semantic difference
   from normal `trie` nodes, but provides slots that allow for a backpointer to the parent
   context, a unique identifier, and potentially a name and property list metadata."
  trie name plist
  (up (car *id*))
  (id (sb-ext:atomic-incf (cdr *id*))))

(defun make-trie ()
  "Makes a new empty `trie`."
  (cons +no-value+ nil))

(defun deref (trie-designator)
  (if (fern-p trie-designator) (fern-trie trie-designator) trie-designator))

(defun make-fern (&optional name up plist)
  "Creates a new, empty `root-fern`, initializing the `name` and context plist metadata"
  (%make-fern :name name :up up :plist plist :trie (make-trie)))

(alias make make-fern)

(defun trie-value (trie)
  "Returns the value located at a `trie` node."
  (declare (cons trie))
  (car trie))

(defun value (trie &aux (trie (deref trie)))
  (trie-value trie))

(defun set-trie-value (trie value)
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
  (let ((trie (if (fern-p trie) (fern-trie trie) trie)))
        (set-trie-value (find-trie key t nil trie) value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-trie-arc (key trie)
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

(defun trie-follow-arc (component extend? trie)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find-Trie, Find-Trie-List are the work horses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-trie (key extend? path trie)
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

(defun find-trie-list (list extend? path trie)
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
			;; Set arc or stash.  We guarantee that only one is
			;; there!
			(if (trie-arcs (cdr stash))
			    (setf (trie-arcs new-for-stash)
				  (trie-arcs (cdr stash)))
			    (setf (trie-stash new-for-stash)
				  (trie-stash (cdr stash))))
			;; now try to index list again.  trie no longer
			;; has a stash.
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
		    (cdr (setf (trie-stash trie)
			       (cons list (make-trie))))))))))

(defun maptrie (f trie &optional (parent nil supplied-p))
  "Calls the function `f` on each key/value pair stored in the
   `trie`.  Note that this is fairly expensive -- the key must be
   delinearized.  If you don't really need to use the key for the values in
   the trie, you should use `maptrie-values`."
  (let* ((trie  (if (fern-p trie) (fern-trie trie) trie))
        (value (trie-value trie)))
    (when (and supplied-p (not (eql value +no-value+)))
      (funcall f (delinearize-sexpr (reverse parent)) value))
    (cond
      ((trie-stash trie)
       (maptrie f (cdr (trie-stash trie))
		(cons (car (trie-stash trie)) parent)))
      ((trie-arcs-hashed-p trie)
       (maphash
	#'(lambda (key node)
	    (maptrie f node (cons (reconstitute-from-hash-key key) parent)))
	(trie-arcs trie)))
      (t
       (let ((arcs (trie-arcs trie)))
	 (when arcs
	 (if (null (cdr arcs))
	     ;; Make it tail recursive if there is only one arc!
	     (maptrie f (cdr (car arcs)) (cons (car (car arcs)) parent))
	     (loop for (key . node) in (trie-arcs trie)
		   do (maptrie f node (cons key parent))))))))))

(defun maptrie-nodes (f trie &optional (with-values t))
  "Calls the function `f` on each `trie` node.  If
   `with-values` is true, the default, then only calls
   `f` on nodes with a value."
  (let* ((trie  (if (fern-p trie) (fern-trie trie) trie)))
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
	       (maptrie-nodes f (cdr (car arcs)) with-values))))))))

(defun maptrie-nodes-remove-if (f trie &optional (key nil))
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

(defmethod print-object ((fern fern) stream)
  (print-unreadable-object (fern stream :type t :identity nil)
    (format stream "~A:~A [~d/~d]"
            (or (if (fern-up fern) (fern-name (fern-up fern))) :_)
            (or (fern-name fern) (format nil "ID~D" (fern-id fern)))
            (trie-node-count (fern-trie fern))
            (trie-node-count (fern-trie fern) t))))  ;; todo: fix this

(defun trie-all-values (trie &aux (trie (deref trie)))
  "Returns a list of all of the values in the `trie`."
  (let ((results nil))
    (maptrie-values #'(lambda (v) (push v results)) trie)
    results))

(alias all-values trie-all-values)

(defun remove-duplicates-equal-using-trie (list)
  "Takes a list and does an `equal` `remove-duplicates`
   on it using a `trie` as an intermediate data structure.  This
   algorithm will be O(n) in the length of the list, and will be much faster
   than using a hash table for the same purpose as long as the elements in the
   list are potentially non-trivial."
  (let ((trie (make-trie)) (unique nil))
    (loop for x in list
	  for n = (find-trie x t nil trie)
	  do (when (eql (trie-value n) +no-value+)
	       (push x unique)
	       (set-trie-value n t)))
    (values unique trie)))

(defun remove-duplicates-using-trie-and-coercion
    (list function &optional (incoming-unique nil) (incoming-trie (make-trie)))
  "Removes the duplicates in `list` coercing the elements in the
   list first using the `function`.  `Incoming-unique`
   is an optional incoming uniquified list, all of whose elements are already in
   `incoming-trie`.  These arguments are used to do multiple,
   repeated remove-duplicates calls over lots of lists.
   Returns three values:
     - The new list of unique elements
     - The number of unique elements found this time.
     - The outgoing modified `trie`."
  (let ((unique incoming-unique)
	(trie incoming-trie)
	(count 0))
    (loop for x in list
	  for coerced = (funcall function x)
	  for n = (find-trie coerced t nil trie)
	  do (when (eq (trie-value n) +no-value+)
	       (push x unique)
	       (set-trie-value n t)))
    (values unique count trie)))

(defun remove-duplicates-equal-using-trie* (lists)
  "This function is just like `remove-duplicates-equal-using-trie`,
   only it takesd a list of lists, and returns a list containing only the
   unique elements unioned across all of the lists."
  (let ((trie (make-trie))
	(unique nil))
    (loop for list in lists
	  do
	  (loop for x in list
		for n = (find-trie x t nil trie)
		do (when (eql (trie-value n) +no-value+)
		     (push x unique)
		     (set-trie-value n t))))
    (values unique trie)))

(defun dismantle-trie (trie &aux (trie (deref trie)))
  "Destructively dismantles the `trie` and all of its inferiors.
   There is likely to be no real need for this unless the user has little
   faith in the GC, or pointers were returned to interior nodes."
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

(defstruct trie-remove-duplicate-state
  (trie (make-trie))
  (unique nil)
  (tail nil)
  (count 0)
  (coercer nil))

(defmethod print-object ((x trie-remove-duplicate-state) (stream t))
 (print-unreadable-object (x stream :type t :identity t)
			  (format stream "~D elements"
			    (trie-remove-duplicate-state-count x))))

;; (defdoc trie-remove-duplicate-state (structure)
;;   "A data structure used to support stateful remove-duplicates behavior.
;;    This is used by operations such as `pushnew-using-state`, and
;;    `remove-duplicates-using-state`.")

;; (defdoc make-trie-remove-duplicate-state (function)
;;   "Constructor for `trie-remove-duplicate-state`s.")

;; (defdoc trie-remove-duplicate-state-count (function)
;;   "The number of objects held within the
;;    `trie-remove-duplicate-state`.")

;; (defdoc trie-remove-duplicate-state-unique (function)
;;   "The list or unique elements held within the
;;    `trie-remove-duplicate-state`.")

(defun member-using-state (x state)
  "Given an object `x` and a state object `state`, which
   is an instance of `remove-duplicates-using-state`, is true
   if `x` has already been recorded in the state."
  ;; Extract stuff from the struct
  (let ((function (trie-remove-duplicate-state-coercer state))
	(trie   (trie-remove-duplicate-state-trie   state)))
    (let ((coerced (if function (funcall function x) x)))
      (multiple-value-bind (n found-p) (get-trie coerced trie)
			   (and found-p (not (eq n +no-value+)))))))

(defun pushnew-using-state (x state &optional (stop-at nil))
  "Given an object `x` and a state object `state`, which
   is an instance of `remove-duplicates-using-state`, adds the
   element `x` to the state if it isn't already there according to
   an `equal` test.  If `stop-at` is an integer then
   the value is not added of the number of values already found in the state
   exceeds the `stop-at` value.  If `state` is nil, a new
   `remove-duplicates-using-state` object is created and returned
   as the fourth value.

   Returns five values:
   <OL>
   <LI>The current list of items in the state.
   <LI>The number of items in the state.
   <LI>A flag that is true is the `stop-at` has been reached.
   <LI>The state object.
   <LI>A flag that is true of the argument `x` was found in the
       state.
   </OL>"
  (when (not state) (setq state (make-trie-remove-duplicate-state)))
  ;; Extract stuff from the struct
  (let ((function (trie-remove-duplicate-state-coercer state))
	(unique (trie-remove-duplicate-state-unique state))
	(count  (trie-remove-duplicate-state-count  state))
	(tail   (trie-remove-duplicate-state-tail   state))
	(trie   (trie-remove-duplicate-state-trie   state))
	(stopped-p nil)
	(found-p t))
    (if (and stop-at (not (eq :all stop-at))
	     (>= (the fixnum count) (the fixnum stop-at)))
	(setq stopped-p t)
	(let ((coerced (if function (funcall function x) x)))
	  (let ((n (find-trie coerced t nil trie)))
	    (when (eq (trie-value n) +no-value+)
	      (set-trie-value n x)
	      (setq found-p nil)
	      (let ((new (list x)))
		(if tail
		    (progn (setf (rest tail) new)
			   (setq tail new))
		    (progn (setq unique new)
			   (setq tail new))))
	      (incf (the fixnum count))
	      (when (and stop-at
			 (>= (the fixnum count) (the fixnum stop-at)))
		(setq stopped-p t))))))
    ;; Now, put them back.
    (setf (trie-remove-duplicate-state-unique state) unique)
    (setf (trie-remove-duplicate-state-count  state) count)
    (setf (trie-remove-duplicate-state-tail   state) tail)
    (values unique count stopped-p state found-p)))

(defun remove-using-state (x state)
  "Given an object `x` and a state object `state`, which
   is an instance of `remove-duplicates-using-state`, removes the
   element `x` from the state if it is already there according to
   an `equal` test.

   Returns five values:
   <OL>
   <LI>The current list of items in the state.
   <LI>The number of items in the state.
   <LI>NIL
   <LI>The state object.
   <LI>A flag that is true of the argument `x` was found in the
       state.
   </OL>"
  (let ((function (trie-remove-duplicate-state-coercer state))
	(unique (trie-remove-duplicate-state-unique state))
	(count  (trie-remove-duplicate-state-count  state))
	(tail   (trie-remove-duplicate-state-tail   state))
	(trie   (trie-remove-duplicate-state-trie   state))
	(found-p nil))
    (let ((coerced (if function (funcall function x) x)))
      (let ((n (find-trie coerced t nil trie)))
	(when (not (eq (trie-value n) +no-value+))
	  (let ((eq-value (trie-value n)))
	  (set-trie-value n +no-value+)
	  (loop with last = nil
		for loc on unique
		when (eq eq-value (first loc))
		do (progn (if last
			      (progn (setf (rest last) (rest loc))
				     (if (rest loc)
					 nil
					 (setq tail loc)))
			      (progn (setq unique (rest loc))
				     (if (rest loc)
					 nil
					 (setq tail nil))))
			  (return nil)))
	  (setq found-p t)
	  (decf (the fixnum count))))))
    ;; Now, put them back.
    (setf (trie-remove-duplicate-state-unique state) unique)
    (setf (trie-remove-duplicate-state-count  state) count)
    (setf (trie-remove-duplicate-state-tail   state) tail)
    (values unique count nil state found-p)))

(defun remove-duplicates-using-state (list state &optional (stop-at nil))
  "Given a list `list` and a state object `state`, which
   is an instance of `remove-duplicates-using-state`, removes
   all of the ruplicates in the `list` according to
   an `equal` test.  If `stop-at` is an integer then
   nothing happens if the number of values already in the state
   exceeds the `stop-at` value.

   Returns four values:
   <OL>
   <LI>The current list of items in the state.
   <LI>The number of items in the state.
   <LI>A flag that is true is the `stop-at` has been reached.
   <LI>The state object.
   </OL>"
  ;; Extract stuff from the struct
  (let ((function (trie-remove-duplicate-state-coercer state))
	(unique (trie-remove-duplicate-state-unique state))
	(count  (trie-remove-duplicate-state-count  state))
	(tail   (trie-remove-duplicate-state-tail   state))
	(trie   (trie-remove-duplicate-state-trie   state))
	(stopped-p nil))
    (if (and stop-at (not (eq :all stop-at))
	     (>= (the fixnum count) (the fixnum stop-at)))
	(setq stopped-p t)
	(loop for x in list
	      for coerced = (funcall function x)
	      for n = (find-trie coerced t nil trie)
	      do (when (eq (trie-value n) +no-value+)
		   (set-trie-value n t)
		   (let ((new (list x)))
		     (if tail
			 (progn (setf (rest tail) new)
				(setq tail new))
			 (progn (setq unique new)
				(setq tail new))))
		   (incf (the fixnum count))
		   (when (and stop-at
			      (>= (the fixnum count) (the fixnum stop-at)))
		     (setq stopped-p t)
		     (return nil)))))
    ;; Now, put them back.
    (setf (trie-remove-duplicate-state-unique state) unique)
    (setf (trie-remove-duplicate-state-count  state) count)
    (setf (trie-remove-duplicate-state-tail   state) tail)
    (values unique count stopped-p state)))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Linearizing sexpressions.
;;
;; linearize-sexpr and delinearize-sexpr use the same linearization as
;; the trie, except that linearize-sexpr linearizes the complete
;; expression, rather than doing it lazily.  The delinearize-sexpr
;; will, however, handle lazy linearization.
;;

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
`(a b c)`.  An
error is signalled if `X` does not represent a complete sexpression."
  (multiple-value-bind (key rest)
      (delinearize-sexpr-1 x)
    (assert (null rest))
    key))

(defun delinearize-sexpr-1 (x)
  (cond
    ((atom x)
     (values x nil))
    ((eq (first x) +start+)
     (delinearize-sexpr-rest (cdr x)))
    (t (values (car x) (cdr x)))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Map over the keys and values in a TRIE
;;

(defgeneric reconstitute-from-hash-key (x)
  (:documentation "The inverse operation of `fast-hash-key`.
   Takes a fast hash key and returns the associated object."))

(defmethod reconstitute-from-hash-key ((x t)) x)

;;; For bound uninterned symbols use the symbol value.  This is the
;;; lucid-specific use of a symbol as a fast hash key.
;;; See also fast-hash-key
#+Lucid
(defmethod reconstitute-from-hash-key ((x symbol))
  (if (and (not (symbol-package x)) (boundp x))
      (symbol-value x)
      x))



#|
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

(defparameter *forms* nil)

(defun get-ol-forms ()
  (setq *forms* nil)
  (loop for o in (oli::all-ontologies)
	do
	(print o)
	(when (oli::definitions o)
	  (maphash
	   #'(lambda (k v) (push (oli::source v) *forms*))
	   (oli::definitions o))))
  (length *forms*))

(defun get-atom (n)
  (ecase n
    (0 'a)
    (1 'b)
    (2 'c)
    (3 42)
    (4 (read-from-string "43.1"))
    (5 (read-from-string "\"s\""))
    (6 (oli::find-word 'ol-user::class 'ol-user::frame-ontology))
    (7 (oli::find-word 'ol-user::function 'ol-user::frame-ontology))))

(defun build-random-tree (&optional (depth 0))
  (let ((key (random (+ depth 3))))
    (case key
      (0 (list (build-random-tree (+ depth 1))
	       (build-random-tree (+ depth 1))))
      (1 (cons (build-random-tree (+ depth 1))
	       (build-random-tree (+ depth 1))))
      (otherwise (get-atom (random 8))))))

(with-substitution-groups
     ((("STRUCTURE-TRIE" "TRIE")))

(defun trie-dbg (list indices &rest trietst-keys)
  (apply 'trietst
	 (loop for (lo hi) in indices
	       append (subseq list lo hi)) trietst-keys))

(defparameter *trie* nil)
(defun trietst (list &key (max (length list)) (check :after) (trace nil)
		   (map nil)(keep nil))
  (ecase check (:after) (:during)((nil)))
  ;; touch the list
  ;;(loop for x in list do (length x))
  (let ((some-trie (make-trie)))
    (when keep (setq *trie* some-trie))
    (time
     (loop for clause in list
	   for i from 0 below max
	   for trie = (find-trie clause t nil some-trie)
	   do
	   ;;(user::pp clause)
	   ;;(user::pp some-trie)
	   (when trace
	     (print "Looking for:") (user::pp clause)
	     (print "Found:")
	     (user::pp trie))

	   (set-trie-value trie
			   (if (eql (trie-value trie) +no-value+)
			       (list i)
			       (cons i (trie-value trie))))
	   (when trace
	     (print "After inserting")
	     (user::pp some-trie))
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

(defun trie-log (list n)
  (start-backtrace-logging "/tmp/log")
  (let ((some-trie (make-trie)))
    (loop for clause in list
	  for i from 0
	  do
	  (push i (get-trie clause some-trie)))
    (loop for i below n
	  do
	  (loop for x in list
		for i from 0
		unless (member i (get-trie x some-trie))
		do (break "Did not find ~s" x))))
  (stop-backtrace-logging)
  (summarize-backtrace-logging
   "/tmp/log" :root 'trie-log :max-backtrace-depth 20))


(defun trie-test-zz (&optional (quiet-p nil))
  (let ((trie (make-trie))
	(facts-so-far (make-hash-table :test #'equal)))
    (setq *trie* trie)
    (loop for key = (build-random-tree)
	  for value = (build-random-tree)
	  collect key into keys
	  do
	  (setf (get-trie key trie) value)
	  (setf (gethash key facts-so-far) value)
	  (let ((found nil))
	    (maptrie #'(lambda (k v)
		       (when (not quiet-p) (format t "~%~S:	~S" k v))
		       (push (list k v) found))
		   *trie*)
	    (when quiet-p (princ "."))
	    (let ((list-of-facts nil))
	      (maphash #'(lambda (k v)(push (list k v) list-of-facts))
		       facts-so-far)
	      (let ((diff1 (set-difference list-of-facts found :test #'equal))
		    (diff2 (set-difference found list-of-facts :test #'equal)))
		(when diff1
		  (format t "~%;;; Not found: ~{~%~S~}" diff1))
		(when diff2
		  (format t "~%;;; Shouldn't have found: ~{~%~S~}" diff2))
		(when (or diff1 diff2) (break))))))))



|#
