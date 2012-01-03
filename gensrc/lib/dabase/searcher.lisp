(defpackage dabase.searcher
  (:use :common-lisp)
  (:shadow :common-lisp get load)
  (:export load
           element-count
           member?
           get
           each-common-prefix))
(in-package :dabase.searcher)

(eval-when (:compile-toplevel :load-toplevel)
  (rename-package :dabase.octet-stream :dabase.octet-stream '(:octet-stream)))

(deftype node () '(unsigned-byte 32))

(defstruct da
  (entry-count 0 :type fixnum)
  (nodes #() :type (simple-array node)))

(defmethod print-object ((o da) stream)
  (print-unreadable-object (o stream :identity t)
    (format stream "~a ~s ~a" :da :count (da-entry-count o))))

(defun read-uint (byte-width in)
  (loop FOR i FROM (1- byte-width) DOWNTO 0
        SUM (ash (read-byte in) (* i 8))))

(defun read-nodes (count in)
  (let ((nodes (make-array count :element-type 'node)))
    (dotimes (i count nodes)
      (setf (aref nodes i) (read-uint 4 in)))))

(defun load (index-file-path)
  (with-open-file (in index-file-path :element-type '(unsigned-byte 8))
    (let ((count (read-uint 4 in)))
      (make-da :entry-count (read-uint 4 in)
               :nodes (read-nodes count in)))))

(defun element-count (da)
  (da-entry-count da))

(defun base (da node)
  (ldb (byte 24 0) (aref (da-nodes da) node)))

(defun value (da node)
  (ldb (byte 24 0) (aref (da-nodes da) node)))

(defun chck (da node)
  (ldb (byte 8 24) (aref (da-nodes da) node)))

(defun member? (key da start end)
  (not (null (get key da start end))))

(defun get (key da start end)
  (labels ((recur (in node &aux (arc (octet-stream:read in)))
             (let ((next (+ (base da node) arc)))
               (when (= (chck da next) arc)
                 (if (= arc 0)
                     (value da node)
                   (recur in next))))))
    (recur (octet-stream:make key :start start :end end) 0)))

(defun each-common-prefix (fn key da start end)
  (labels ((recur (in node &aux (arc (octet-stream:read in)))
             (when (= (chck da (+ (base da node) 0)) 0)
               (funcall fn (value da node) (octet-stream:position in)))
             (let ((next (+ (base da node) arc)))
               (when (and (= (chck da next) arc)
                          (/= arc 0))
                 (recur in next)))))
    (recur (octet-stream:make key :start start :end end) 0)))

(eval-when (:compile-toplevel :load-toplevel)
  (rename-package :dabase.octet-stream :dabase.octet-stream '()))
