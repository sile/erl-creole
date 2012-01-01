(defpackage dabase.builder
  (:use :common-lisp)
  (:export build))
(in-package :dabase.builder)

(deftype node () '(unsigned-byte 32))

(eval-when (:compile-toplevel :load-toplevel)
  (rename-package :dabase.octet-stream :dabase.octet-stream '(:octet-stream))
  (rename-package :dabase.node-allocator :dabase.node-allocator '(:node-allocator)))

(defconstant +EMPTY+ #xFFFFFFFF)

#|
DA & NODE
|#
(defstruct da
  (nodes #() :type (simple-array node))
  (entries #() :type simple-vector)
  (alloca t :type node-allocator:node-allocator))
(defmethod print-object ((o da) stream)
  (print-unreadable-object (o stream :identity t :type t)))

(defun init-da (entries)
  (make-da :nodes (make-array 64 :element-type 'node :initial-element +EMPTY+)
           :entries (map 'vector (lambda (e) (list (octet-stream:make (first e)) (second e))) entries)
           :alloca (node-allocator:make)))

(defun ensure-index (da index)
  (with-slots (nodes) (the da da)
    (when (<= (length nodes) index)
      (setf nodes (adjust-array nodes (* (length nodes) 2) :initial-element +EMPTY+))
      (ensure-index da index))))

(defun node-ref (da index)
  (ensure-index da index)
  (aref (da-nodes da) index))

(defun (setf node-ref) (node da index)
  (ensure-index da index)
  (setf (aref (da-nodes da) index) node))

(defun build (entries output-file)
  (let ((da (init-da entries)))
    (build-impl da 0 #1=(length entries) 0)
    (output-to-file da #1# output-file)
    :done))

(defun get-key (da index)
  (first (aref (da-entries da) index)))

(defun get-value (da index)
  (second (aref (da-entries da) index)))

(defun node-alloc (da arcs)
  (node-allocator:allocate (da-alloca da) arcs))

(defun (setf get-base) (value da index)
  (declare ((unsigned-byte 24) value))
  (setf (ldb (byte 24 0) (node-ref da index)) value))

(defun (setf get-chck) (chck da index)
  (declare ((unsigned-byte 8) chck))
  (setf (ldb (byte 8 24) (node-ref da index)) chck))

(defun set-node (da node-index base-node-index arc)
  (let ((next-index (+ base-node-index arc)))
    (setf (get-base da node-index) base-node-index
          (get-chck da next-index) arc)
    next-index))

#|
BUILD
|#
(defun end-of-same-node (da beg end)
  (loop WITH ch = (octet-stream:read (get-key da beg))
        FOR cur FROM (1+ beg) BELOW end
        WHILE (= ch (octet-stream:peek (get-key da cur)))
    DO (octet-stream:read (get-key da cur))
    FINALLY
    (return cur)))

(defun build-leaf-case (da entry-index node-index)
  (loop WITH key = (get-key da entry-index)
        UNTIL (octet-stream:eos? key)
        FOR arc = (octet-stream:read key)
    DO
    (setf node-index (set-node da node-index (node-alloc da `(,arc)) arc))
    FINALLY
    (setf node-index (set-node da node-index (node-alloc da `(,octet-stream:null-octet))
                                              octet-stream:null-octet)
          (get-base da node-index) (get-value da entry-index))))

(defun build-node-case (da beg end root-node-index) 
  (let ((children '())
        (ranges '()))
    (loop WHILE (/= beg end)
      DO
      (push (octet-stream:peek (get-key da beg)) children)
      (push beg ranges)
      (setf beg (end-of-same-node da beg end))
      FINALLY
      (setf children (nreverse children)
            ranges (nreverse (cons end ranges))))
;    (print `(:c ,children))
;    (sleep 1)
    (let ((base-node-index (node-alloc da children)))
      (loop FOR child IN children
            FOR child-beg IN ranges
            FOR child-end IN (cdr ranges)
        DO
;        (print (list :r child-beg child-end))
        (build-impl da child-beg child-end (set-node da root-node-index base-node-index child))))))

(defun build-impl (da beg end root-node-index) 
  (declare (da da)
           (fixnum beg end)
           (node root-node-index))
  (if (= (- end beg) 1)
      (build-leaf-case da beg root-node-index)
    (build-node-case da beg end root-node-index)))

#|
OUTPUT
|#
(defun write-uint (n byte-size out)
  (loop FOR i FROM (1- byte-size) DOWNTO 0
        DO
        (write-byte (ldb (byte 8 (* i 8)) n) out)))

(defun output-to-file (da entry-count output-file)
  (with-open-file (out output-file :direction :output
                                   :element-type '(unsigned-byte 8)
                                   :if-exists :supersede)
    (let ((end (+ (position-if-not (lambda (x) (= x +EMPTY+)) (da-nodes da) :from-end t) #x100)))
      (write-uint end 4 out)
      (write-uint entry-count 4 out)
      (loop FOR i FROM 0 BELOW end
            FOR n = (aref (da-nodes da) i)
            DO (write-uint n 4 out)))))

(eval-when (:compile-toplevel :load-toplevel)
  (rename-package :dabase.octet-stream :dabase.octet-stream '())
  (rename-package :dabase.node-allocator :dabase.node-allocator '()))
