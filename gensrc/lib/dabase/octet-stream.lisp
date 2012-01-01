(defpackage dabase.octet-stream
  (:use :common-lisp)
  (:shadow :common-lisp read position)
  (:export make
           make-impl
           read
           peek
           eos?
           position

           null-octet))
(in-package :dabase.octet-stream)

(defconstant null-octet 0) ; xxx:

(defgeneric make (seq &key start end))
(defgeneric make-impl (type seq &key start end))
(defgeneric eos? (stream))
(defgeneric read (stream))
(defgeneric peek (stream))
(defgeneric position (stream))

(defun seq-type (seq)
  (etypecase seq
    (string :string)
    ((simple-array (unsigned-byte 8)) :octets)))

(defmethod make (seq &key start end)
  (make-impl (seq-type seq) seq :start start :end end))
