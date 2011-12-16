;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;
;;;;; FERN is a fusion of the unicly:unique-universal-identifier framework with
;;;;; OKBC-TRIES, an efficient, sexp-based (consp), key-value storage and
;;;;; indexing abstraction that 

(in-package :fern)

;; There are three easy ways to linearize lists:
;; (p a (b . c))
;; 1. (cons p cons a cons cons b cons c cons nil)
;; 2. (start p a start b dot c dot nil)
;; 3. (start p a start b dot c end)
;;
;; The original trie code in Norvig uses (1).  Both (2) and (3) are more
;; compact, with (3) being the most compact.  Because the extra code
;; to handle (3) is truly minimal, and it is general and (always) more
;; compact, we use it instead.
;;
;; Stashing.  We now (9/24/97) do not linearize the entire list, but
;; only as much as we need to.  The rest is stashed.
;; The above list might be linearized as:
;; 4a. (start (p a (b . c)))
;; 4b. (start p a ((b . c)))
;; 4c. (start p a start (b . c))
;; depending on the contents.  The 'stash' holds the rest of a list,
;; which already has a 'start' node.
;;
;; This is *much* more compact for the test cases that I've looked at
;; so far.  Typical improvements are a factor of 10-20 in storage
;; usage.  E.g. from 18mb to 0.45mb.  This has a significant impact on
;; GC behavior.


(defmacro defdoc (sym (type) &body strings)
  "Defines a documentation method for the symbol <sym> on the documentation
   type <code>type</code>."
  (assert (not (eq type 'class)))
  #+ignore
  (let ((meth `(defmethod documentation
		((sym (eql ',sym)) &optional (type nil))
		(if (eq type ',type)
		    ,(first strings)
		    (call-next-method)))))
    meth)
  (let ((body `(progn (setf (documentation ',sym ',type) ,(first strings))
		',sym)))
    #+Harlequin-Common-Lisp `(eval-when (compile load eval) ,body)
    #-Harlequin-Common-Lisp body))

(defmacro defokbcgeneric (name (&rest args) &body options)
  "The way to define defgeneric forms in the OKBC implementation."
  #+(or (not Lucid)
	generic-function-doc-string-patch-loaded)
  `(defgeneric ,name (,@args) ,@options)
  ;; Code around a Lucid bug that blows away defgeneric doc strings in
  ;; compiled files.
  #+(and Lucid (not generic-function-doc-string-patch-loaded))
  (let ((doc nil)
	(filtered-opts nil))
    (loop for opt in options
	  do (if (or (not (consp opt))
		     (not (eq :documentation (first opt))))
		 (push opt filtered-opts)
		 (setq doc (second opt))))
    (if doc
	`(progn
	  (defgeneric ,name (,@args) ,@filtered-opts)
	  ,@(if doc `((defdoc ,name (function) ,doc)) nil)
	  #',name)
	;; Just a bit neater.
	`(defgeneric ,name (,@args) ,@options))))


(defokbcgeneric fast-hash-key (thing)
  (:documentation "In some Lisp implementations, some data structures hash
   much faster or more evenly than others.  For example, symbols might have
   better hashing behavior than CLOS instances.  Because of this difference
   in performance, a number of places in the OKBC implementation indirect
   before hashing occurs from objects to the hash keys that give faster hashing
   for those objects.  <P>

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
    ((consp x)
     (and (consp y)
	  (equal (car (the cons x))(car (the cons y)))))
    (t nil)))

(defconstant +start+ '*START*
    "Special value to indicate the start of a list.")
(defconstant +end+   '*END*
    "Special value to indicate the end of a list.")
(defconstant +dot+   '*DOT*
    "Special value to indicate a dotted pair.")

(defconstant +no-value+ '*NO-VALUE*
  "Special value to indicate that the value cell of a <code>trie</code> empty.")

(defparameter *list-to-hashtable-threshold* 20)
(defparameter *initial-hash-table-size* 500)
(defparameter *rehash-size* 3.0)
(defparameter *id* 0)

(defstruct structure-trie
  ;;(trie (:type list)) (id (incf *id*))
  (value +no-value+)
  (arcs nil)
  (stash nil))

(defdoc structure-trie (structure)
  "The tries implementation provides two distinct implementations of the trie
   data structure, one implented as cons cells, and called <code>trie</code>s,
   and the other implemented as defstructs, and called
   <code>structure-trie</code>s.  <code>Structure-trie</code>s use more
   space, but are much easier to understand in the debugger end inspector.
   If you think you might want to debug a <code>trie</code> data structure
   for any reason, you should use this one until fully debugged.")

(defdoc make-structure-trie (function)
  "Makes a new empty <code>structure-trie</code> node.")

(defdoc structure-trie-p (function)
  "A predicate that is true of <code>structure-trie</code> nodes.")

(defdoc structure-trie-value (function)
  "Accessor to get to the value slot of a <code>structure-trie</code> node.")

(defdoc structure-trie-arcs (function)
  "Accessor to get to the arcs dangling from a given a
   <code>structure-trie</code> node.")

(defstruct (root-trie (:include structure-trie))
  purpose
  up)

(defdoc root-trie (structure)
  "A defstruct for the root <code>trie</code> node for a <code>trie</code>
   discrimination net.  This class has no semantic difference from normal
   <code>trie</code> nodes, but it has slots that allow for a backpointer to
   the object containing the trie (<code>up</code>) and a reason for the
   trie being created in the first place (<code>purpose</code>).  These are
   useful in debugging.")

(defdoc make-root-trie (function)
  "Makes a new empty <code>root-trie</code>.  Initargs are <code>:up</code>
   and <code>:purpose</code>.")

(defdoc root-trie-purpose (function)
  "An accessor for the <code>purpose</code> slot in a <code>root-trie</code>.")

(defdoc root-trie-up (function)
  "An accessor for the <code>up</code> slot in a <code>root-trie</code>.")

(defmethod print-object ((object root-trie) stream)
  (print-unreadable-object
   (object stream :type t :identity t)
   (prin1 (root-trie-purpose object) stream)))

(defun new-root-trie (purpose up)
  "Creates a new, empty <code>root-trie</code>, initializing the <code>up</code>
   and <code>purpose</code> slots accordingly."
  (make-root-trie :purpose purpose :up up))

  ;; To save space, each trie is single cons cell
  ;;  (value . arcs), or
  ;;  (value . (stash . (stashed-list . trie)))
  
(defun make-trie ()
  "Makes a new empty <code>trie</code>."
  (cons +no-value+ nil))


(defun trie-value (trie)
  "Returns the value located at a <code>trie</code> node."
  (declare (cons trie))
  (car trie))

(defun (setf trie-value) (value trie)
  (declare (cons trie))
  (setf (car trie) value))

(defun trie-arcs (trie)
  "Returns the list of arcs dangling from a given trie node."
  (declare (cons trie))
  (let ((x (cdr trie)))
    (if (and (consp x) (eql (car x) 'STASH))
	nil x)))

(defun (setf trie-arcs) (value trie)
  (declare (cons trie))
  ;; (assert (not (member 'stash trie)))
  (setf (cdr trie) value))

(defun trie-stash (trie)
  (declare (cons trie))
  (let ((x (cdr trie)))
    (if (and (consp x) (eql (car x) 'STASH))
	(cdr x) nil)))

(defun (setf trie-stash) (value trie)
  (declare (cons trie))
  (if (null value)
      (let ((x (cdr trie)))
	(when (eql (car x) 'STASH)
	  (setf (cdr trie) nil)))
      (setf (cdr trie) (cons 'STASH value)))
  value)

(defun share-one-arc (l1 l2)
  (or (eql l1 l2)
      (and (consp l1) (consp l2))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lexical substitution machinery
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(arnesi:eval-always 
(defun case-replace (from to)
  (cond ((string= (string-upcase from) from) (string-upcase to))
	((string= (string-downcase from) from) (string-downcase to))
	((string= (string-capitalize from) from) (string-capitalize to))
	(t (string-capitalize to))))

(defun string-subst (new old string)
  (let ((index (search (string old) string :test #'char-equal)))
    (if index
	(concatenate 'string
	 (subseq string 0 index)
	 (case-replace
	  (subseq string index (+ index (length (string old)))) (string new))
	 (string-subst
	  new old (subseq string (+ index (length (string old))))))
	string)))

(defun string-subst-symbol (new old symbol)
  (let ((new-string (string-subst (string-upcase new) (string-upcase old)
				  (symbol-name symbol))))
    (intern new-string (symbol-package symbol))))

(defun substitute-within-sexpression (new old tree)
  (typecase tree
    (cons (cons (substitute-within-sexpression new old (first tree))
		(substitute-within-sexpression new old (rest  tree))))
    (symbol (typecase old
	      (symbol (if (eq old tree) new tree))
	      (string (if (search old (string tree) :test #'char-equal)
			  (string-subst-symbol new old tree)
			  tree))
	      (otherwise tree)))
    (string (typecase old
	      ((or string symbol) (string-subst new old tree))
	      (otherwise tree)))
    (otherwise tree)))

(defun substitute-all-within-sexpression (news-and-olds tree)
  (if news-and-olds
      (substitute-all-within-sexpression
       (rest news-and-olds)
       (substitute-within-sexpression
	(first (first news-and-olds)) (second (first news-and-olds)) tree))
      tree))

(defmacro with-substitution-groups  ((&rest substitution-alists) &body body)
  "A utility macro that takes a number of forms in <code>body</code> and
   replicates that code substituting the symbols and strings provided in
   <code>substitution-alists</code>.  This is useful is a chunk of code is
   going to be basically the same but for a few tiny changes for a number
   of different cases.  Using this macros lets you substitute accessor names
   within methods and such like.  For example:<PRE>
   (with-substitution-groups (((\"STRUCTURE-TUPLE-KB\" \"TUPLE-KB\")))
     (defmethod get-foo ((kb tuple-kb))
        (tuple-kb-foo kb)))</PRE> will expand into:
   <PRE>   (PROGN (DEFMETHOD GET-FOO ((KB TUPLE-KB))
            (TUPLE-KB-FOO KB))
          (DEFMETHOD GET-FOO ((KB STRUCTURE-TUPLE-KB))
            (STRUCTURE-TUPLE-KB-FOO KB)))</PRE>"
  `(progn
    ,@body
    ,@(loop for news-and-olds in substitution-alists
	    append (substitute-all-within-sexpression
		    news-and-olds body))))
)

;==============================================================================
 ;;; START OF SUBSTITUTION GROUP !!!!!!
;------------------------------------------------------------------------------
(with-substitution-groups
     ((("STRUCTURE-TRIE" "TRIE")))

(defun trie-arcs-hashed-p (trie)
  "A predicate that is trie of a <code>trie</code> node if the node has had
   enough arcs depending on it to push it into hashed mode."
  (not (listp (trie-arcs trie))))

(defun set-trie-value (trie value) ;; hook
  "Sets the value slot at a given <code>trie</code> node."
  (setf (trie-value trie) value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface functions

(defun delete-trie (key trie)
  "Marks the entry for <code>key</code> in the <code>trie</code> as deleted.
   This does *not* remove the key from the trie. Returns T if key was in the
   trie, else nil."
  (let ((key-trie (find-trie key nil nil trie)))
    (when key-trie
      (set-trie-value key-trie +no-value+))
    (if key-trie t nil)))

(defun get-trie (key trie)
  "Returns the value for a <code>key</code> in a <code>trie</code>, and t/nil
   if found.  This function is equivalent to <code>gethash</code>.  The second
   value returned is true if an entry was found."
  (let* ((key-trie (find-trie key nil nil trie))
	 (val (when key-trie (trie-value key-trie))))
    (if (or (null key-trie) (eq val +no-value+))
	(values nil nil)
      (values val t))))

(defun get-trie-returning-node
    (key trie &optional (default nil))
  "Returns the value for a <code>key</code> in a <code>trie</code>, and t/nil
   if found.  The second value returned is true if an entry was found, and
   false otherwise.  The third value is the trie node locating the key in the
   trie's net, so that we can do a modify on the locative without having to
   do the get again.  This function has uses analogous to the old modify-hash
   of Zetalisp."
  (let* ((key-trie (find-trie key t nil trie))
	 (val (when key-trie (trie-value key-trie))))
    (if (or (null key-trie) (eq val +no-value+))
	(values default nil key-trie)
	(values val t key-trie))))

(defun get-trie-returning-node-no-extend
    (key trie &optional (default nil))
  "Returns the value for a <code>key</code> in a <code>trie</code>, and t/nil
   if found.  The second value returned is true if an entry was found, and
   false otherwise.  The third value is the trie node so that we can do a
   modify on the locative without having to do the get again.  If the node is
   not found, we do NOT extend, so no the third returned value is not
   defined.  See also <code>get-trie-returning-node</code>."
  (let* ((key-trie (find-trie key nil nil trie))
	 (val (when key-trie (trie-value key-trie))))
    (if (or (null key-trie) (eq val +no-value+))
	(values default nil key-trie)
	(values val t key-trie))))

(defun (setf get-trie) (value key trie)
  "Set the value of key in trie."
    (set-trie-value (find-trie key t nil trie) value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal auxilliary functions

;; took some optimizations form Jim's string=-trie
(defun get-trie-arc (key trie)
  (declare (optimize (speed 3) (safety 0)
		     #+lucid (compilation-speed 0)
		     #+EXCL (debug 0)))
  (if (trie-arcs-hashed-p trie)
      (let ((hash-key (fast-hash-key (the atom key))))
	#-Lucid
	(gethash hash-key (the hash-table (trie-arcs trie)))
	#+Lucid
	(if (symbolp hash-key)
	    (lucid:gethash-fast-for-symbol
	     hash-key (the hash-table (trie-arcs trie)))
	    (gethash hash-key (the hash-table (trie-arcs trie)))))
      ;; cdr is a trie, can't be nil.
      (if (stringp key)
	  (loop for entry in (the list (trie-arcs trie))
		for arc-key = (car (the cons entry))
		when (and (stringp arc-key)
			  (string= (the string key)
				   (the string arc-key)))
		return (cdr entry))
	  (loop for entry
		in (the list (trie-arcs trie))
		when (eql key (first (the cons entry)))
		return (cdr entry)))))

(defun trie-could-follow-one-arc (key trie)
  (get-trie-arc
   (if (atom key) key +start+)
   trie))

(defun trie-follow-arc (component extend? trie)
  "Find the trie node for this component of the key.
If EXTEND? is true, make a new node when necessary."
  (let ((next-trie (get-trie-arc component trie)))
    (if next-trie
	next-trie
	(if (not extend?)
	    nil
	    (let ((new-trie (make-trie)))
	      (cond
		((trie-arcs-hashed-p trie)
		 ;; add to ht
		 (setf (gethash (fast-hash-key component)
				(trie-arcs trie))
		       new-trie))
		((>= (length (the list (trie-arcs trie)))
		     *list-to-hashtable-threshold*)
		 ;; List is too long, convert to a hashtable
		 ;; (princ "H")
		 (let ((ht (make-hash-table
			    :test #'equal ;because of strings, sigh
			    :rehash-size *rehash-size*
			    :size *initial-hash-table-size*)))
		   (loop for entry in (trie-arcs trie)
			 do
			 (setf (gethash (fast-hash-key
					 (car entry)) ht) 
			       (cdr entry)))
		   (setf (gethash (fast-hash-key component) ht)
			 new-trie)
		   (setf (trie-arcs trie) ht)))
		(t
		 ;; add to list
		 (push (cons component new-trie) (trie-arcs trie))))
	      ;; return the new trie
	      new-trie)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find-Trie, Find-Trie-List do the real work
;;

(defun find-trie (key extend? path trie)
  "Find the trie node for this key.  If EXTEND? is true, make a new
node if necessary."
  (when (not (null trie))
    (if (consp key)
	(find-trie-list key extend? path
		      (find-trie +start+ extend? nil trie))
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
  "Calls the function <code>f</code> on each key/value pair stored in the
   <code>trie</code>.  Note that this is fairly expensive -- the key must be
   delinearized.  If you don't really need to use the key for the values in
   the trie, you should use <code>maptrie-values</code>."
  (let ((value (trie-value trie)))
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
  "Calls the function <code>f</code> on each <code>trie</code> node.  If
   <code>with-values</code> is true, the default, then only calls
   <code>f</code> on nodes with a value."
  (when (or (null with-values)
	    (not (eql (trie-value trie) +no-value+)))
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

(defun maptrie-nodes-remove-if (f trie &optional (key nil))
  "Maps over a <code>trie</code> conditionally removing nodes.  The nodes in
   the <code>trie</code> are visited top-down.  The function <code>f</code> is
   applied to three arguments: a <code>trie</code>, a key, and a value.  The
   key is the label on the arc leading to this <code>trie</code>.  The key will
   be nil for the top level trie.  If <code>f</code> returns true, then the
   subtree under the <code>trie</code> is dismantled and the arc
   leading to it is removed."
  (cond
    ((and key
	  ;;(not (eq trie-deleted value)) ;; snip out even if deleted
	  (funcall #+lucid (the system:procedure f) #-lucid f
		   trie key  (trie-value trie)))
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
		       (delete arc (trie-arcs trie) :count 1)))))
     ;; and return nil
     nil)))

(defun maptrie-values (f trie)
  "Calls the function <code>f</code> on each value stored in the
   <code>trie</code>."
  (maptrie-nodes
   #'(lambda (trie) (funcall f (trie-value trie)))
   trie))

(defun trie-node-count (trie &optional (with-values t))
  "Returns the number of nodes in the <code>trie</code> that have values."
  (let ((max 0)
	(count 0))
    (maptrie-nodes
     #'(lambda (trie)
	 (let ((len (if (trie-arcs-hashed-p trie)
			(hash-table-count (trie-arcs trie))
			(length (trie-arcs trie)))))
	   (incf count)
	   (setq max (max max len))))
     trie with-values)
    (values count max)))

(defun trie-all-values (trie)
  "Returns a list of all of the values in the <code>trie</code>."
  (let ((results nil))
    (maptrie-values #'(lambda (v) (push v results)) trie)
    results))

(defun remove-duplicates-equal-using-trie (list)
  "Takes a list and does an <code>equal</code> <code>remove-duplicates</code>
   on it using a <code>trie</code> as an intermediate data structure.  This
   algorithm will be O(n) in the length of the list, and will be much faster
   than using a hash table for the same purpose as long as the elements in the
   list are potentially non-trivial."
  (let ((trie (make-trie))
	(unique nil))
    (loop for x in list
	  for n = (find-trie x t nil trie)
	  do (when (eql (trie-value n) +no-value+)
	       (push x unique)
	       (set-trie-value n t)))
    (values unique trie)))

(defun remove-duplicates-using-trie-and-coercion
    (list function &optional (incoming-unique nil) (incoming-trie (make-trie)))
  "Removes the duplicates in <code>list</code> coercing the elements in the
   list first using the <code>function</code>.  <code>Incoming-unique</code>
   is an optional incoming uniquified list, all of whose elements are already in
   <code>incoming-trie</code>.  These arguments are used to do multiple,
   repeated remove-duplicates calls over lots of lists.<P>

   Returns three values:
   <OL>
   <LI>The new list of unique elements
   <LI>The number of unique elements found this time.
   <LI>The outgoing modified <code>trie</code>.
   </OL>"
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
  "This function is just like <code>remove-duplicates-equal-using-trie</code>,
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

(defun dismantle-trie (trie)
  "Destructively dismantles the <code>trie</code> and all of its inferiors.
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

;==============================================================================
) ;;; END OF SUBSTITUTION GROUP !!!!!!
;------------------------------------------------------------------------------
;==============================================================================
;------------------------------------------------------------------------------

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

(defdoc trie-remove-duplicate-state (structure)
  "A data structure used to support stateful remove-duplicates behavior.
   This is used by operations such as <code>pushnew-using-state</code>, and
   <code>remove-duplicates-using-state</code>.")

(defdoc make-trie-remove-duplicate-state (function)
  "Constructor for <code>trie-remove-duplicate-state</code>s.")

(defdoc trie-remove-duplicate-state-count (function)
  "The number of objects held within the
   <code>trie-remove-duplicate-state</code>.")

(defdoc trie-remove-duplicate-state-unique (function)
  "The list or unique elements held within the
   <code>trie-remove-duplicate-state</code>.")

(defun member-using-state (x state)
  "Given an object <code>x</code> and a state object <code>state</code>, which
   is an instance of <code>remove-duplicates-using-state</code>, is true
   if <code>x</code> has already been recorded in the state."
  ;; Extract stuff from the struct
  (let ((function (trie-remove-duplicate-state-coercer state))
	(trie   (trie-remove-duplicate-state-trie   state)))
    (let ((coerced (if function (funcall function x) x)))
      (multiple-value-bind (n found-p) (get-trie coerced trie)
			   (and found-p (not (eq n +no-value+)))))))

(defun pushnew-using-state (x state &optional (stop-at nil))
  "Given an object <code>x</code> and a state object <code>state</code>, which
   is an instance of <code>remove-duplicates-using-state</code>, adds the
   element <code>x</code> to the state if it isn't already there according to
   an <code>equal</code> test.  If <code>stop-at</code> is an integer then
   the value is not added of the number of values already found in the state
   exceeds the <code>stop-at</code> value.  If <code>state</code> is nil, a new
   <code>remove-duplicates-using-state</code> object is created and returned
   as the fourth value.<P>

   Returns five values:
   <OL>
   <LI>The current list of items in the state.
   <LI>The number of items in the state.
   <LI>A flag that is true is the <code>stop-at</code> has been reached.
   <LI>The state object.
   <LI>A flag that is true of the argument <code>x</code> was found in the
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
  "Given an object <code>x</code> and a state object <code>state</code>, which
   is an instance of <code>remove-duplicates-using-state</code>, removes the
   element <code>x</code> from the state if it is already there according to
   an <code>equal</code> test.<P>

   Returns five values:
   <OL>
   <LI>The current list of items in the state.
   <LI>The number of items in the state.
   <LI>NIL
   <LI>The state object.
   <LI>A flag that is true of the argument <code>x</code> was found in the
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
  "Given a list <code>list</code> and a state object <code>state</code>, which
   is an instance of <code>remove-duplicates-using-state</code>, removes
   all of the ruplicates in the <code>list</code> according to
   an <code>equal</code> test.  If <code>stop-at</code> is an integer then
   nothing happens if the number of values already in the state
   exceeds the <code>stop-at</code> value.<P>

   Returns four values:
   <OL>
   <LI>The current list of items in the state.
   <LI>The number of items in the state.
   <LI>A flag that is true is the <code>stop-at</code> has been reached.
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


;------------------------------------------------------------------------------

(defun copy-deep-trie (trie)
  "Given a <code>trie</code> node makes a deep copy of it, copying all nodes
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

(defokbcgeneric copy-deep-structure-trie (structure-trie)
  (:documentation "Given a <code>structure-trie</code> node makes a deep copy
   of it, copying all nodes beneath it, but not copying the values in the
   nodes themselves."))

(defmethod copy-deep-structure-trie ((structure-trie structure-trie))
  (make-structure-trie))

(defmethod copy-deep-structure-trie ((structure-trie root-trie))
  (make-root-trie))

(defmethod copy-deep-structure-trie :around
  ((trie root-trie))
  (let ((new (call-next-method)))
    (setf (root-trie-up new) (root-trie-up trie))
    (setf (root-trie-purpose new) (root-trie-purpose trie))
    new))

(defmethod copy-deep-structure-trie :around
  ((structure-trie structure-trie))
  (let ((new (call-next-method)))
    (set-structure-trie-value new (structure-trie-value structure-trie))
    (if (structure-trie-stash structure-trie)
	(setf (structure-trie-stash new)
	      (cons
	       ;; (copy-tree (car (structure-trie-stash structure-trie)))
	       ;; Don't copy-tree the list structure of the trie stash.  The
	       ;; Goal is to make a new trie with new indexing structure, but
	       ;; we'd still like to structure-share with any non-disassembled
	       ;; list structure.
	       (car (structure-trie-stash structure-trie))
	       (copy-deep-structure-trie
		(cdr (structure-trie-stash structure-trie)))))
	(let ((arcs (structure-trie-arcs structure-trie)))
	  (setf (structure-trie-arcs new)
		(cond
		  ((structure-trie-arcs-hashed-p structure-trie)
		   (let ((ht (make-hash-table
			      :test (hash-table-test arcs)
			      :rehash-size *rehash-size*
			      :size
			      (ceiling (* 1.6 (hash-table-count arcs))))))
		     (maphash
		      #'(lambda (key value)
			  (setf (gethash key ht)
				(copy-deep-structure-trie value)))
		      arcs)
		     ht))
		  (t (loop for (key . value) in
			   arcs
			   collect (cons key
					 (copy-deep-structure-trie
					  value))))))))
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
  "Linearizes an sexpression <code>x</code>.  This is the same representation
   used in the trie indexing structure.  For example, the linearized version of
   the sexpr <code>(a (b) c . d)</code> is
   <code>(*start* a *start* b *end* c *dot* d)</code>.
   Also see <code>delinearize-sexpr</code>."
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
  "<code>X</code> is a linearized sexpression.  Return the sexpression that it
represents.  See <code>linearize-sexpr</code>.  Handles lazy linearization in
the form of non-atomic elements.  E.g., <code>(*start* a (b c))</code> is
<code>(a b c)</code>.  An
error is signalled if <code>X</code> does not represent a complete sexpression."
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

(defokbcgeneric reconstitute-from-hash-key (x)
  (:documentation "The inverse operation of <code>fast-hash-key</code>.
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

) ;; end with-substitution-groups

|#


;;; Now add the reader macro version:

(defokbcgeneric copy-deep-hybrid-trie (trie)
  (:documentation "Given a hybrid <code>trie</code> or
   <code>structure-trie</code> node makes a deep copy of it, copying all nodes
   beneath it, but not copying the values in the nodes themselves."))

(defmethod copy-deep-hybrid-trie ((trie cons))
  (copy-deep-trie trie))

(defmethod copy-deep-hybrid-trie ((trie structure-trie))
  (make-structure-trie))

(defmethod copy-deep-hybrid-trie ((structure-trie root-trie))
  (make-root-trie))

(defmethod copy-deep-hybrid-trie :around
  ((trie root-trie))
  (let ((new (call-next-method)))
    (setf (root-trie-up new) (root-trie-up trie))
    (setf (root-trie-purpose new) (root-trie-purpose trie))
    new))

(defmethod copy-deep-hybrid-trie :around
  ((structure-trie structure-trie))
  (let ((new (call-next-method)))
    (set-structure-trie-value new (structure-trie-value structure-trie))
    (if (structure-trie-stash structure-trie)
	(setf (structure-trie-stash new)
	      (cons
	       ;; (copy-tree (car (structure-trie-stash structure-trie)))
	       ;; Don't copy-tree the list structure of the trie stash.  The
	       ;; Goal is to make a new trie with new indexing structure, but
	       ;; we'd still like to structure-share with any non-disassembled
	       ;; list structure.
	       (car (structure-trie-stash structure-trie))
	       (copy-deep-hybrid-trie
		(cdr (structure-trie-stash structure-trie)))))
	(let ((arcs (structure-trie-arcs structure-trie)))
	  (setf (structure-trie-arcs new)
		(cond
		  ((structure-trie-arcs-hashed-p structure-trie)
		   (let ((ht (make-hash-table
			      :test (hash-table-test arcs)
			      :rehash-size *rehash-size*
			      :size
			      (ceiling (* 1.6 (hash-table-count arcs))))))
		     (maphash
		      #'(lambda (key value)
			  (setf (gethash key ht)
				(copy-deep-hybrid-trie value)))
		      arcs)
		     ht))
		  (t (loop for (key . value) in
			   arcs
			   collect (cons key
					 (copy-deep-hybrid-trie
					  value))))))))
    new))

(defokbcgeneric delete-hybrid-trie (key trie)
  (:documentation "Given a hybrid <code>trie</code> or
   <code>structure-trie</code> node, deletes the node associated with
   <code>key</code>."))

(defmethod delete-hybrid-trie (key (trie cons))
 (delete-trie key trie))

(defmethod delete-hybrid-trie (key (trie structure-trie))
 (let ((key-trie (find-hybrid-trie key nil nil trie)))
   (when key-trie
     (set-hybrid-trie-value key-trie +no-value+))
   (if key-trie t nil)))

(defokbcgeneric dismantle-hybrid-trie (trie)
  (:documentation "Given a hybrid <code>trie</code> or
   <code>structure-trie</code> node, dismantles all nodes hanging beneath it."))

(defmethod dismantle-hybrid-trie ((trie cons))
  (dismantle-trie trie))

(defmethod dismantle-hybrid-trie ((trie structure-trie))
  (set-structure-trie-value trie +no-value+)
  (cond
    ((structure-trie-arcs-hashed-p trie)
     (maphash #'(lambda (key node)
		  (declare (ignore key))
		  (dismantle-hybrid-trie node))
	      (structure-trie-arcs trie))
     (clrhash (structure-trie-arcs trie)))
    ((structure-trie-stash trie)
     (dismantle-hybrid-trie (cdr (structure-trie-stash trie)))
     (setf (structure-trie-stash trie) nil))
    (t
     (let ((arcs (structure-trie-arcs trie)))
       ;; If there is just one arc, then we could be tail-recursive
       (loop for pair in arcs
	     do (dismantle-hybrid-trie (cdr pair)))
       (setf (structure-trie-arcs trie) nil)))))

(defokbcgeneric get-hybrid-trie (key trie)
  (:documentation "Returns the value for a <code>key</code> in a
   <code>trie</code>, and t/nil if found.  The trie may consist of some
   mixture of <code>trie</code> instances and <code>structure-trie</code>
   instances.  This function is equivalent to <code>gethash</code>.  The second
   value returned is true if an entry was found."))

(defmethod get-hybrid-trie ((key t) (trie cons))
  (get-trie key trie))

(defmethod get-hybrid-trie ((key t) (trie structure-trie))
  (let* ((key-trie (find-hybrid-trie key nil nil trie))
	 (val (when key-trie (hybrid-trie-value key-trie))))
    (if (or (null key-trie) (eq val +no-value+))
	(values nil nil)
      (values val t))))

(defokbcgeneric get-hybrid-trie-returning-node
    (key trie &optional default)
  (:documentation "Returns the value for a <code>key</code> in a
   <code>trie</code>, and t/nil if found.  The trie may consist of some
   mixture of <code>trie</code> instances and <code>structure-trie</code>
   instances.  The second value returned is true if an entry was found, and
   false otherwise.  The third value is the trie node locating the key in the
   trie's net, so that we can do a modify on the locative without having to
   do the get again.  This function has uses analogous to the old modify-hash
   of Zetalisp."))

(defmethod get-hybrid-trie-returning-node
 ((key t) (trie cons) &optional (default nil))
  (get-trie-returning-node key trie default))

(defmethod get-hybrid-trie-returning-node
 ((key t) (trie structure-trie) &optional (default nil))
  (let* ((key-trie (find-hybrid-trie key t nil trie))
	 (val (when key-trie (hybrid-trie-value key-trie))))
    (if (or (null key-trie) (eq val +no-value+))
	(values default nil key-trie)
	(values val t key-trie))))

(defokbcgeneric get-hybrid-trie-returning-node-no-extend
    (key trie &optional default)
  (:documentation "Returns the value for a <code>key</code> in a
   <code>trie</code>, and t/nil if found.  The trie may consist of some
   mixture of <code>trie</code> instances and <code>structure-trie</code>
   instances.  The second value returned is true if an entry was found, and
   false otherwise.  The third value is the trie node so that we can do a
   modify on the locative without having to do the get again.  If the node is
   not found, we do NOT extend, so no the third returned value is not
   defined.  See also <code>get-hybrid-trie-returning-node</code>."))

(defmethod get-hybrid-trie-returning-node-no-extend
 ((key t) (trie cons) &optional (default nil))
  (get-trie-returning-node-no-extend key trie default))

(defmethod get-hybrid-trie-returning-node-no-extend
 ((key t) (trie structure-trie) &optional (default nil))
  (let* ((key-trie (find-hybrid-trie key nil nil trie))
	 (val (when key-trie (hybrid-trie-value key-trie))))
    (if (or (null key-trie) (eq val +no-value+))
	(values default nil key-trie)
	(values val t key-trie))))

(defmacro make-hybrid-trie (&rest inits)
  "Makes either a <code>trie</code> or a <code>structure-trie</code>
   depending on the setting of the <code>use-minimal-tries</code> feature.
   The data structure created using this constructor should only be accessed
   using the <code>hybrid-</code> operations."
  #+use-minimal-tries `(make-trie ,@inits)
  #-use-minimal-tries `(make-structure-trie ,@inits))

(defokbcgeneric maphybrid-trie (f trie &optional parent)
  (:documentation "Calls the function <code>f</code> on each key/value pair
   stored in the <code>trie</code>.  The trie may consist of some mixture of
   <code>trie</code> instances and <code>structure-trie</code> instances.
   Note that this is fairly expensive -- the key must be
   delinearized.  If you don't really need to use the key for the values in
   the trie, you should use <code>maptrie-values</code>."))

(defmethod maphybrid-trie (f (trie cons) &optional (parent nil supplied-p))
  (if supplied-p
      (maptrie f trie parent)
      (maptrie f trie)))

(defmethod maphybrid-trie
 (f (trie structure-trie) &optional (parent nil supplied-p))
  (let ((value (structure-trie-value trie)))
    (when (and supplied-p (not (eql value +no-value+)))
      (funcall f (delinearize-sexpr (reverse parent)) value))
    (cond
      ((structure-trie-stash trie)
       (maphybrid-trie f (cdr (structure-trie-stash trie))
		       (cons (car (structure-trie-stash trie)) parent)))
      ((structure-trie-arcs-hashed-p trie)
       (maphash
	#'(lambda (key node)
	    (maptrie f node (cons (reconstitute-from-hash-key key) parent)))
	(structure-trie-arcs trie)))
      (t
       (let ((arcs (structure-trie-arcs trie)))
	 (when arcs
	 (if (null (cdr arcs))
	     ;; Make it tail recursive if there is only one arc!
	     (maphybrid-trie f (cdr (car arcs)) (cons (car (car arcs)) parent))
	     (loop for (key . node) in (structure-trie-arcs trie)
		   do (maphybrid-trie f node (cons key parent))))))))))

(defokbcgeneric maphybrid-trie-nodes (f trie &optional with-values)
  (:documentation "Calls the function <code>f</code> on each <code>trie</code>
   node.  If <code>with-values</code> is true, the default, then only calls
   <code>f</code> on nodes with a value.  The trie may consist of some mixture
   of <code>trie</code> instances and <code>structure-trie</code> instances."))

(defmethod maphybrid-trie-nodes (f (trie cons) &optional (with-values t))
  (maptrie-nodes f trie with-values))

(defmethod maphybrid-trie-nodes
 (f (trie structure-trie) &optional (with-values t))
  (when (or (null with-values)
	    (not (eql (structure-trie-value trie) +no-value+)))
    (funcall f trie))
  (cond
    ((structure-trie-arcs-hashed-p trie)
     (maphash #'(lambda (key node)
		  (declare (ignore key))
		  (maphybrid-trie-nodes f node))
	      (structure-trie-arcs trie)))
    ((structure-trie-stash trie)
     (maphybrid-trie-nodes f (cdr (structure-trie-stash trie))) with-values)
    (t
     (let ((arcs (structure-trie-arcs trie)))
       ;; If there is just one arc, then we can be tail-recursive
       (when arcs
	 (if (cdr (structure-trie-arcs trie))
	     (loop for pair in arcs
		   do (maphybrid-trie-nodes f (cdr pair) with-values))
	     (maphybrid-trie-nodes f (cdr (car arcs)) with-values)))))))

(defokbcgeneric maphybrid-trie-nodes-remove-if (f trie &optional key)
  (:documentation "Maps over a <code>trie</code> conditionally removing nodes.
   The nodes in the <code>trie</code> are visited top-down.  The trie may
   consist of some mixture of <code>trie</code> instances and
   <code>structure-trie</code> instances.  The function <code>f</code> is
   applied to three arguments: a <code>trie</code>, a key, and a value.  The
   key is the label on the arc leading to this <code>trie</code>.  The key will
   be nil for the top level trie.  If <code>f</code> returns true, then the
   subtree under the <code>trie</code> is dismantled and the arc
   leading to it is removed."))

(defmethod maphybrid-trie-nodes-remove-if
 (f (trie cons) &optional (key nil))
  (maptrie-nodes-remove-if f trie key))

(defmethod maphybrid-trie-nodes-remove-if
 (f (trie structure-trie) &optional (key nil))
  (cond
    ((and key
	  ;;(not (eq trie-deleted value)) ;; snip out even if deleted
	  (funcall #+lucid (the system:procedure f) #-lucid f
		   trie key  (structure-trie-value trie)))
     ;; passed the test, dismantle the inferiors and return T
     (dismantle-hybrid-trie trie) T)
    (t
     ;; this trie is ok, but recurse. 
     (cond
       ((structure-trie-arcs-hashed-p trie)
	(maphash #'(lambda (arc-key arc-trie)
		     (when (maphybrid-trie-nodes-remove-if
			    f arc-trie arc-key)
		       (remhash arc-key (structure-trie-arcs trie))))
		 (structure-trie-arcs trie)))
       ((structure-trie-stash trie)
	(when (maphybrid-trie-nodes-remove-if
	       f (cdr (structure-trie-stash trie))
	       (car (structure-trie-stash trie)))
	  (setf (structure-trie-stash trie) nil)))
       (t
	(loop for arc in (structure-trie-arcs trie)
	      for result = (maphybrid-trie-nodes-remove-if
			    f (rest arc) (first arc))
	      when result
	      do (setf (structure-trie-arcs trie)
		       (delete arc (structure-trie-arcs trie) :count 1)))))
     ;; and return nil
     nil)))

(defun maphybrid-trie-values (f trie)
  "Calls the function <code>f</code> on each value stored in the
   <code>trie</code>.  The trie may consist of some mixture of
   <code>trie</code> instances and <code>structure-trie</code> instances."
  (maphybrid-trie-nodes
   #'(lambda (trie) (funcall f (hybrid-trie-value trie)))
   trie))

(defokbcgeneric set-hybrid-trie-value (trie value)
  (:documentation "Sets the value slot at a given <code>trie</code> node.
   The trie may consist of some mixture of <code>trie</code> instances and
   <code>structure-trie</code> instances."))

(defmethod set-hybrid-trie-value ((trie cons) (value t))
  (set-trie-value trie value))

(defmethod set-hybrid-trie-value ((trie structure-trie) (value t))
  (set-structure-trie-value trie value))

(defun hybrid-trie-all-values (trie)
  "Returns a list of all of the values in the <code>trie</code>.
   The trie may consist of some mixture of <code>trie</code> instances and
   <code>structure-trie</code> instances."
  (let ((results nil))
    (maphybrid-trie-values #'(lambda (v) (push v results)) trie)
    results))

(defokbcgeneric hybrid-trie-arcs (trie)
  (:documentation "Returns the list of arcs dangling from a given trie node.
   The trie may consist of some mixture of <code>trie</code> instances and
   <code>structure-trie</code> instances."))

(defmethod hybrid-trie-arcs ((trie cons))
  (trie-arcs trie))

(defmethod hybrid-trie-arcs ((trie structure-trie))
  (structure-trie-arcs trie))

(defmethod (setf  hybrid-trie-arcs) ((new-value t) (trie cons))
  (setf (trie-arcs trie) new-value))

(defmethod (setf hybrid-trie-arcs) ((new-value t) (trie structure-trie))
  (setf (structure-trie-arcs trie) new-value))

(defokbcgeneric hybrid-trie-arcs-hashed-p (trie)
  (:documentation "A predicate on <code>trie</code> nodes that is true is the
   number of dependent arcs has grown large enough that it has flipped into
   hashing mode.  The trie may consist of some mixture of <code>trie</code>
   instances and <code>structure-trie</code> instances."))

(defmethod hybrid-trie-arcs-hashed-p ((trie cons))
  (trie-arcs-hashed-p trie))

(defmethod hybrid-trie-arcs-hashed-p ((trie structure-trie))
  (structure-trie-arcs-hashed-p trie))

(defun hybrid-trie-node-count (trie &optional (with-values t))
  "Returns the number of nodes in the <code>trie</code> that have values.
   When <code>with-values</code> is true, returns only the number of nodes
   that value associated values.
   The trie may consist of some mixture of <code>trie</code> instances and
   <code>structure-trie</code> instances."
  (let ((max 0)
	(count 0))
    (maphybrid-trie-nodes
     #'(lambda (trie)
	 (let ((len (if (hybrid-trie-arcs-hashed-p trie)
			(hash-table-count (hybrid-trie-arcs trie))
			(length (hybrid-trie-arcs trie)))))
	   (incf count)
	   (setq max (max max len))))
     trie with-values)
    (values count max)))

(defokbcgeneric hybrid-trie-value (trie)
  (:documentation "Returns the value located at a <code>trie</code> node.
   The trie may consist of some mixture of <code>trie</code> instances and
   <code>structure-trie</code> instances."))

(defmethod hybrid-trie-value ((trie cons))
  (trie-value trie))

(defmethod hybrid-trie-value ((trie structure-trie))
  (structure-trie-value trie))

(defmethod (setf hybrid-trie-value) ((new-value t) (trie cons))
  (setf (trie-value trie) new-value))

(defmethod (setf hybrid-trie-value) ((new-value t) (trie structure-trie))
  (setf (structure-trie-value trie) new-value))

(defokbcgeneric hybrid-trie-stash (trie)
  (:documentation "Returns the stashed tail of the list in the trie.
   The trie may consist of some mixture of <code>trie</code> instances and
   <code>structure-trie</code> instances."))

(defmethod hybrid-trie-stash ((trie cons))
  (trie-stash trie))

(defmethod hybrid-trie-stash ((trie structure-trie))
  (structure-trie-stash trie))

(defmethod (setf hybrid-trie-stash) ((new-stash t) (trie cons))
  (setf (trie-stash trie) new-stash))

(defmethod (setf hybrid-trie-stash) ((new-stash t) (trie structure-trie))
  (setf (structure-trie-stash trie) new-stash))

(defun find-hybrid-trie (key extend? path hybrid-trie)
  "Find the hybrid-trie node for this key.  If EXTEND? is true, make a new
   node if necessary."
  (when (not (null hybrid-trie))
    (if (consp key)
	(find-hybrid-trie-list
	 key extend? path
	 (find-hybrid-trie +start+ extend? nil hybrid-trie))
	(hybrid-trie-follow-arc key extend? hybrid-trie))))

(defun find-hybrid-trie-list-no-stash (list extend? path hybrid-trie)
  (cond
    ((null list)
     (find-hybrid-trie +end+ extend? path hybrid-trie))
    ((atom list)
     (find-hybrid-trie list extend? path
		       (find-hybrid-trie +dot+ extend? nil hybrid-trie)))
    (t
     (find-hybrid-trie-list
      (cdr list) extend?
      (if (car-equal list path) (cdr path) nil)
      (find-hybrid-trie
       (car list) extend? (and (consp path) (car path)) hybrid-trie)))))

(defun find-hybrid-trie-list (list extend? path hybrid-trie)
  (if (null hybrid-trie)
      (if extend? (error "shouldn't be here") nil)
      (let ((stash (hybrid-trie-stash hybrid-trie)))
	(if stash
	    (if (and list (equal list (car stash)))
		(cdr stash)
		(if (not extend?)
		    (find-hybrid-trie-list-no-stash list nil nil hybrid-trie)
		    (progn
		      (setf (hybrid-trie-stash hybrid-trie) nil)
		      (let ((new-for-stash
			     (find-hybrid-trie-list
			      (cdr (car stash)) t
			      (if (car-equal list (car stash)) (cdr list) nil)
			      (find-hybrid-trie (car (car stash))
					 'FORCE (and (consp list) (car list))
					 hybrid-trie))))
			(set-hybrid-trie-value
			 new-for-stash (hybrid-trie-value (cdr stash)))
			;; Set arc or stash.  We guarantee that only one is
			;; there!
			(if (hybrid-trie-arcs (cdr stash))
			    (setf (hybrid-trie-arcs new-for-stash)
				  (hybrid-trie-arcs (cdr stash)))
			    (setf (hybrid-trie-stash new-for-stash)
				  (hybrid-trie-stash (cdr stash))))
			;; now try to index list again.  hybrid-trie no longer
			;; has a stash.
			(find-hybrid-trie-list-no-stash
			 list t path hybrid-trie)))))
	    (if (not extend?)
		(find-hybrid-trie-list-no-stash list nil nil hybrid-trie)
		(if (or (eql extend? 'FORCE)
			(not (consp list)) ; don't stash atoms!
			(hybrid-trie-arcs hybrid-trie)
			(share-one-arc	; don't stash if list matchs path
			 (car (the cons list)) (and (consp path) (car path))))
		    ;; don't want to stash here
		    (find-hybrid-trie-list-no-stash list t path hybrid-trie)
		    ;; do want to stash here
		    (cdr (setf (hybrid-trie-stash hybrid-trie)
			       (cons list (make-hybrid-trie))))))))))


(defun hybrid-trie-follow-arc (component extend? hybrid-trie)
  "Find the hybrid-trie node for this component of the key.
   If EXTEND? is true, make a new node when necessary."
  (let ((next-hybrid-trie (get-hybrid-trie-arc component hybrid-trie)))
    (if next-hybrid-trie
	next-hybrid-trie
	(if (not extend?)
	    nil
	    (let ((new-hybrid-trie (make-hybrid-trie)))
	      (cond
		((hybrid-trie-arcs-hashed-p hybrid-trie)
		 ;; add to ht
		 (setf (gethash (fast-hash-key component)
				(hybrid-trie-arcs hybrid-trie))
		       new-hybrid-trie))
		((>= (length (the list (hybrid-trie-arcs hybrid-trie)))
		     *list-to-hashtable-threshold*)
		 ;; List is too long, convert to a hashtable
		 ;; (princ "H")
		 (let ((ht (make-hash-table
			    :test #'equal ;because of strings, sigh
			    :rehash-size *rehash-size*
			    :size *initial-hash-table-size*)))
		   (loop for entry in (hybrid-trie-arcs hybrid-trie)
			 do
			 (setf (gethash (fast-hash-key
					 (car entry)) ht) 
			       (cdr entry)))
		   (setf (gethash (fast-hash-key component) ht)
			 new-hybrid-trie)
		   (setf (hybrid-trie-arcs hybrid-trie) ht)))
		(t
		 ;; add to list
		 (push (cons component new-hybrid-trie)
		       (hybrid-trie-arcs hybrid-trie))))
	      ;; return the new hybrid-trie
	      new-hybrid-trie)))))

(defun get-hybrid-trie-arc (key hybrid-trie)
  (declare (optimize (speed 3) (safety 0)
		     #+lucid (compilation-speed 0)
		     #+EXCL (debug 0)))
  (if (hybrid-trie-arcs-hashed-p hybrid-trie)
      (let ((hash-key (fast-hash-key (the atom key))))
	#-Lucid
	(gethash hash-key (the hash-table (hybrid-trie-arcs hybrid-trie)))
	#+Lucid
	(if (symbolp hash-key)
	    (lucid:gethash-fast-for-symbol
	     hash-key (the hash-table (hybrid-trie-arcs hybrid-trie)))
	    (gethash hash-key
		     (the hash-table (hybrid-trie-arcs hybrid-trie)))))
      ;; cdr is a hybrid-trie, can't be nil.
      (if (stringp key)
	  (loop for entry in (the list (hybrid-trie-arcs hybrid-trie))
		for arc-key = (car (the cons entry))
		when (and (stringp arc-key)
			  (string= (the string key)
				   (the string arc-key)))
		return (cdr entry))
	  (loop for entry
		in (the list (hybrid-trie-arcs hybrid-trie))
		when (eql key (first (the cons entry)))
		return (cdr entry)))))

(defun new-root-hybrid-trie (purpose up)
  "The behavior of this function depends on the setting of the
   <code>use-minimal-tries</code> feature.  If this feature is set,
   a <code>trie</code> will be created, and the <code>up</code>
   and <code>purpose</code> values will be ignored.  If the feature is not
   set, a new, empty <code>root-trie</code>, initializing the <code>up</code>
   and <code>purpose</code> slots accordingly."
  #+use-minimal-tries (declare (ignore purpose up))
  #+use-minimal-tries (make-trie)
  #-use-minimal-tries
  (make-root-trie :purpose purpose :up up))

(defun (setf get-hybrid-trie) (value key trie)
  "Set the value of key in trie."
    (set-hybrid-trie-value (find-hybrid-trie key t nil trie) value))

