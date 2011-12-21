;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;
;;; Adapted from apparently dormant github project  quek/rucksack-mmap

(in-package :planks.btree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Octets, Vectors, Buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-buffer (size)
  (make-array size :element-type '(unsigned-byte 8)))


(defun unsigned-byte-to-vector (unsigned-byte size)
  (let ((buffer (make-buffer size)))
    (loop for i from 0 below size
          do (setf (aref buffer i)
                   (ldb (byte 8 (* i 8)) unsigned-byte)))
    buffer))


(defun vector-to-unsigned-byte (vector size)
  (loop for i from 0 below size
        with n = 0
        do (setf n (dpb (aref vector i) (byte 8 (* i 8)) n))
        finally (return n)))


(defun copy-sap-to-vector (sap sap-start vector vector-start length)
  (typecase vector
    (simple-vector
       (sb-sys::with-pinned-objects (sap vector)
         (sb-kernel::system-area-ub8-copy sap sap-start
                                          (sb-sys::vector-sap vector) vector-start
                                          length)))
    (t
       (loop repeat length
             for i from sap-start
             for j from vector-start
             do (setf (aref vector j)
                      (sb-sys:sap-ref-8 sap i))))))


(defun copy-vector-to-sap (vector vector-start sap sap-start length)
  (typecase vector
    (simple-vector
       (sb-sys::with-pinned-objects (vector sap)
         (sb-kernel::system-area-ub8-copy (sb-sys::vector-sap vector) vector-start
                                          sap sap-start
                                          length)))
    (t
       (loop repeat length
             for i from vector-start
             for j from sap-start
             do (setf (sb-sys:sap-ref-8 sap j)
                      (aref vector i))))))


(defun copy-sap-to-sap (sap-src sap-src-start sap-dest sap-dest-start length)
  (sb-sys::with-pinned-objects (sap-src sap-dest)
    (sb-kernel::system-area-ub8-copy sap-src sap-src-start
                                     sap-dest sap-dest-start
                                     length)))


(defun string-to-octets (string)
  (sb-ext:string-to-octets string :external-format :utf-8))


(defun octets-to-string (octets &key end)
  (sb-ext:octets-to-string octets :external-format :utf-8 :end end))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; memory mapped gray-streams
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass mmap-stream (sb-gray:fundamental-binary-input-stream
                       sb-gray:fundamental-binary-output-stream)
  ((base-stream  :initarg :base-stream)
    (mmap-size   :initarg :mmap-size)
    (file-length :initform 0)
    (position    :initform 0)
    (sap         :reader mmap-stream-sap)
    (ext         :initarg :ext :initform nil)
    (lock        :initform (make-recursive-spinlock))))


(defmethod initialize-instance :after ((stream mmap-stream) &key)
  (with-slots (base-stream file-length mmap-size sap) stream
    (setf file-length (file-length base-stream)
          sap (sb-posix:mmap nil
                             mmap-size
                             (boole boole-ior sb-posix:prot-read sb-posix:prot-write)
                             sb-posix:map-shared
                             (sb-sys:fd-stream-fd base-stream)
                             0))))


(defmethod close ((stream mmap-stream) &key abort)
  (with-slots (base-stream mmap-size sap) stream
    (sb-posix:munmap sap mmap-size)
    (close base-stream :abort abort)))


(defmethod stream-truncate ((stream mmap-stream) size)
  (with-slots (base-stream file-length) stream
    (setf file-length size)
    (sb-posix:ftruncate base-stream size)))


(defmacro define-write-method (name
                               (&rest lambda-list)
                               length
                               under-mmap-size
                               over-mmap-size
                               cross-over-mmap-size
                               return-value)
  `(defmethod ,name ,lambda-list
     (with-slots (base-stream file-length mmap-size sap position ext lock) stream
       (with-recursive-spinlock (lock)
         (let* ((length ,length)
                (end-position (+ length position)))
           (flet ((ensure-file-length ()
                    (let ((current-len file-length))
                      (when (< current-len end-position)
                        (stream-truncate stream
                                         (if ext
                                             (max end-position
                                                  (min mmap-size (ceiling (* current-len ext))))
                                             end-position))))))
             (cond ((< end-position mmap-size)
                    (ensure-file-length)
                    ,under-mmap-size)
                   ((<= mmap-size position)
                    ,over-mmap-size)
                   (t
                    (ensure-file-length)
                    ,cross-over-mmap-size))
             (setf position end-position)
             ,return-value))))))


(defmacro define-read-method (name
                              (&rest lambda-list)
                              length
                              under-mmap-size
                              over-mmap-size
                              cross-over-mmap-size)
  `(defmethod ,name ,lambda-list
     (with-slots (base-stream mmap-size sap position lock) stream
       (with-recursive-spinlock (lock)
         (let ((length ,length))
           (prog1
               (cond ((< (+ position length) mmap-size)
                      ,under-mmap-size)
                     ((<= mmap-size position)
                      ,over-mmap-size)
                     (t
                      ,cross-over-mmap-size))
             (incf position length)))))))


(define-write-method sb-gray:stream-write-sequence ((stream mmap-stream)
                                                    (buffer sequence)
                                                    &optional (start 0) end)
  (if end (- end start) (length buffer))
  (copy-vector-to-sap buffer start sap position length)
  (progn
    (file-position base-stream position)
    (write-sequence buffer base-stream)
    (setf file-length end-position))
  (let ((mlen (- mmap-size position)))
    (copy-vector-to-sap buffer start sap position mlen)
    (file-position base-stream (1- mmap-size))
    (write-sequence buffer base-stream :start (1- mlen) :end end))
  buffer)


(define-read-method sb-gray:stream-read-sequence ((stream mmap-stream)
                                                  (buffer sequence)
                                                  &optional (start 0) end)
  (progn
    (unless end (setf end (length buffer)))
    (- end start))
  (progn
    (copy-sap-to-vector sap position buffer start length)
    end)
  (progn
    (file-position base-stream position)
    (read-sequence buffer base-stream))
  (let ((mlen (- mmap-size position)))
    (copy-sap-to-vector sap position buffer start mlen)
    (file-position base-stream (1- mmap-size))
    (read-sequence buffer base-stream :start (1- mlen) :end end)))

(define-write-method sb-gray:stream-write-byte ((stream mmap-stream) integer)
  1
  (setf (sb-sys:sap-ref-8 sap position) integer)
  (progn
    (file-position base-stream position)
    (write-byte integer base-stream))
  ()
  integer)

(define-read-method sb-gray:stream-read-byte ((stream mmap-stream))
  1
  (sb-sys:sap-ref-8 sap position)
  (progn
    (file-position base-stream position)
    (read-byte base-stream))
  ())

(defmacro define-write-unsigned-byte-method (name size)
  `(define-write-method ,name (integer (stream mmap-stream))
     ,size
     (setf (sb-sys:sap-ref-64 sap position) integer)
     (let ((buffer (unsigned-byte-to-vector integer ,size)))
       (file-position base-stream position)
       (write-sequence buffer base-stream))
     (let ((mlen (- mmap-size position))
           (buffer (unsigned-byte-to-vector integer ,size)))
       (copy-vector-to-sap buffer 0 sap position mlen)
       (file-position base-stream (1- mmap-size))
       (write-sequence buffer base-stream :start (1- mlen) :end ,size))
     integer))


(defmacro define-read-unsigned-byte-method (name size)
  `(define-read-method ,name ((stream mmap-stream))
     ,size
     (sb-sys:sap-ref-64 sap position)
     (let ((buffer (make-buffer ,size)))
       (file-position base-stream position)
       (read-sequence buffer base-stream :end ,size)
       (vector-to-unsigned-byte buffer ,size))
     (let ((mlen (- mmap-size position))
           (buffer (make-buffer ,size)))
       (copy-sap-to-vector sap position buffer 0 mlen)
       (file-position base-stream (1- mmap-size))
       (read-sequence buffer base-stream :start (1- mlen) :end ,size)
       (vector-to-unsigned-byte buffer ,size))))


(define-write-unsigned-byte-method write-unsigned-byte-64 8)
(define-write-unsigned-byte-method write-unsigned-byte-32 4)
(define-write-unsigned-byte-method write-unsigned-byte-16 2)
(define-read-unsigned-byte-method read-unsigned-byte-64 8)
(define-read-unsigned-byte-method read-unsigned-byte-32 4)
(define-read-unsigned-byte-method read-unsigned-byte-16 2)


(defmethod read-seq-at (stream sequence position &key (start 0) end)
  (with-slots (lock) stream
    (with-recursive-spinlock (lock)
      (file-position stream position)
      (read-sequence sequence stream :start start :end end))))


(defmethod write-seq-at (stream sequence position &key (start 0) end)
  (with-slots (lock) stream
    (with-recursive-spinlock (lock)
      (file-position stream position)
      (write-sequence sequence stream :start start :end end))))


(defmacro define-stream-at-method (name (&rest args) &body body)
  `(defmethod ,name (stream position ,@args)
     (with-slots (lock) stream
       (with-recursive-spinlock (lock)
         (file-position stream position)
         ,@body))))


(define-stream-at-method write-8-at  (integer) (write-byte integer stream))
(define-stream-at-method write-16-at (integer) (write-unsigned-byte-16 integer stream))
(define-stream-at-method write-32-at (integer) (write-unsigned-byte-32 integer stream))
(define-stream-at-method write-64-at (integer) (write-unsigned-byte-64 integer stream))
(define-stream-at-method read-8-at  () (read-byte stream))
(define-stream-at-method read-16-at () (read-unsigned-byte-16 stream))
(define-stream-at-method read-32-at () (read-unsigned-byte-32 stream))
(define-stream-at-method read-64-at () (read-unsigned-byte-64 stream))


(defmethod sb-gray:stream-file-position ((stream mmap-stream) &optional position-spec)
  (with-slots (base-stream position) stream
    (if position-spec
        (setf position
              (case position-spec
                (:start 0)
                (:end
                   (file-position base-stream :end)
                   (file-position base-stream))
                (t position-spec)))
        position)))


(defmethod stream-length ((stream mmap-stream))
  (with-slots (file-length) stream
    file-length))


(defgeneric stream-pathname (stream)
  (:method ((stream mmap-stream))
    (with-slots (base-stream) stream
      (pathname base-stream)))
  (:method (stream)
      (pathname stream)))


(defun open-mmap-stream (path mmap-size &optional (extend 1.5))
  (make-instance 'mmap-stream
                 :base-stream (open path :direction :io :element-type '(unsigned-byte 8)
                                    :if-exists :overwrite :if-does-not-exist :create)
                 :mmap-size mmap-size :ext extend))




#|
(let ((f (open "/tmp/a.txt" :direction :io :element-type '(unsigned-byte 8)
               :if-exists :overwrite :if-does-not-exist :create)))
  (with-open-stream (m (make-instance 'mmap-stream :base-stream f :mmap-size 5))
    (stream-truncate m 7)
    (let ((code (char-code #\!)))
      (file-position m 1)
      (write-byte code m)
      (file-position m 1)
      (assert (= code (read-byte m))))
    (let ((buffer (make-array 3 :element-type '(unsigned-byte 8))))
      (file-position m 0)
      (write-sequence (sb-ext:string-to-octets "abc") m)
      (file-position m 0)
      (read-sequence buffer m)
      (assert (equalp (sb-ext:string-to-octets "abc") buffer) (buffer) "1 ~a" buffer))
    (let ((buffer (make-array 3 :element-type '(unsigned-byte 8))))
      (write-sequence (sb-ext:string-to-octets "def") m)
      (file-position m 3)
      (read-sequence buffer m)
      (assert (equalp (sb-ext:string-to-octets "def") buffer) (buffer) "2 ~a" buffer))
    (let ((buffer (make-array 3 :element-type '(unsigned-byte 8))))
      (write-sequence (sb-ext:string-to-octets "ghi") m)
      (file-position m 6)
      (read-sequence buffer m)
      (assert (equalp (sb-ext:string-to-octets "ghi") buffer) (buffer) "3 ~a" buffer))
    (let ((code (char-code #\#)))
      (write-byte code m)
      (file-position m (1- (file-position m)))
      (assert (= code (read-byte m))))
    (let ((buffer1 (make-array 20 :element-type '(unsigned-byte 8) :initial-element 1))
          (buffer2 (make-array 20 :element-type '(unsigned-byte 8))))
      (file-position m 0)
      (write-sequence buffer1 m)
      (file-position m 0)
      (read-sequence buffer2 m)
      (assert (equalp buffer1 buffer2) (buffer1 buffer2) "3 ~a" buffer))))
|#
