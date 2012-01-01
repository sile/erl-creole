(defpackage dabase.byte-octet-stream
  (:use :common-lisp)
  (:shadow :common-lisp read position)
  (:export make
           read
           peek
           eos?
           position

           null-octet))
(in-package :dabase.byte-octet-stream)

(defconstant null-octet 0)

;;;;;;;;
;;; type
(deftype array-index () '(mod #.array-total-size-limit))
(deftype bytes () '(simple-array (unsigned-byte 8)))

;;;;;;;;;;;;;;;;
;;; octet-stream
(defstruct octet-stream
  (src #() :type bytes)
  (pos   0 :type array-index)
  (end   0 :type array-index))

;;;;;;;;;;;;;;;;;;;;;
;;; external function
(defun position (in)
  (octet-stream-pos in))

(defun make (bytes &key (start 0) (end (length bytes)))
  (declare (bytes bytes)
           (array-index start end))
  (make-octet-stream :src bytes
                     :pos start
                     :end end))

(defun eos? (in)
  (with-slots (pos end) (the octet-stream in)
    (= pos end)))

(defun peek (in)
  (when (eos? in)
    (return-from peek null-octet))

  (with-slots (src pos) (the octet-stream in)
    (aref src pos)))

(defun eat (in)
  (when (eos? in)
    (return-from eat))

  (with-slots (pos) (the octet-stream in)
    (incf pos))
  in)

(defun read (in)
  (prog1 (peek in)
    (eat in)))

;;;;;;;;;;;;;;;;;;
(defmethod dabase.octet-stream:make-impl ((type (eql :octets)) bytes &key start end)
  (make bytes :start (or start 0) :end (or end (length bytes))))

(defmethod dabase.octet-stream:eos? ((stream octet-stream))
  (eos? stream))

(defmethod dabase.octet-stream:read ((stream octet-stream))
  (read stream))

(defmethod dabase.octet-stream:peek ((stream octet-stream))
  (peek stream))

(defmethod dabase.octet-stream:position ((stream octet-stream))
  (position stream))
