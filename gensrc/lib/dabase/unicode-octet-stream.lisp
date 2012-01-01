(defpackage dabase.unicode-octet-stream
  (:use :common-lisp)
  (:shadow :common-lisp read position)
  (:export make
           read
           peek
           eos?
           position

           null-octet))
(in-package :dabase.unicode-octet-stream)

(defconstant null-octet 0)

;;;;;;;;
;;; type
(deftype unicode () '(mod #.char-code-limit))
(deftype array-index () '(mod #.array-total-size-limit))
(deftype simple-characters () '(simple-array character))

;;;;;;;;;;;;;;;;
;;; octet-stream
(defstruct octet-stream
  (src      "" :type simple-characters)
  (pos       0 :type array-index)
  (end       0 :type array-index)
  (code      0 :type unicode)
  (octet-pos 0 :type (mod 5))
  (octet-len 0 :type (mod 5)))

;;;;;;;;;;;;;;;;;;;;;;
;;; auxiliary function
(defun octet-length (code)
  (declare (unicode code))
  (cond ((< code #x80)    1)
        ((< code #x800)   2)
        ((< code #x10000) 3)
        (t                4)))

;;;;;;;;;;;;;;;;;;;;;
;;; external function
(defun position (in)
  (octet-stream-pos in))

(defun make (string &key (start 0) (end (length string)))
  (declare (simple-characters string)
           (array-index start end))
  (let* ((code (if (= start (length string)) 
                  0
                (char-code (char string start))))
         (len (octet-length code)))
    (make-octet-stream :src string :pos start :end end
                       :code code :octet-pos len :octet-len len)))

(defun eos? (in)
  (with-slots (pos end) (the octet-stream in)
    (= pos end)))

(defun peek (in)
  (when (eos? in)
    (return-from peek null-octet))

  (with-slots (code octet-pos octet-len) (the octet-stream in)
    (if (= octet-pos octet-len)
        (case octet-len
            (1 code)
            (2 (+ #b11000000 (ldb (byte 5  6) code)))
            (3 (+ #b11100000 (ldb (byte 4 12) code)))
            (t (+ #b11110000 (ldb (byte 3 18) code))))
      (+ #b10000000 (ldb (byte 6 (* (the (mod 4) (1- octet-pos)) 6)) code)))))

(defun eat (in)
  (when (eos? in)
    (return-from eat))
  (with-slots (src pos code octet-pos octet-len) (the octet-stream in)
    (decf octet-pos)
    (when (zerop octet-pos)
      (incf pos)
      (unless (eos? in)
        (setf code (char-code (char src pos))
              octet-len (octet-length code)
              octet-pos octet-len))))
  in)

(defun read (in)
  (prog1 (peek in)
    (eat in)))

;;;;;;;;;;;;;;;;;;
(defmethod dabase.octet-stream:make-impl ((type (eql :string)) str &key start end)
  (make str :start (or start 0) :end (or end (length str))))

(defmethod dabase.octet-stream:eos? ((stream octet-stream))
  (eos? stream))

(defmethod dabase.octet-stream:read ((stream octet-stream))
  (read stream))

(defmethod dabase.octet-stream:peek ((stream octet-stream))
  (peek stream))

(defmethod dabase.octet-stream:position ((stream octet-stream))
  (position stream))
