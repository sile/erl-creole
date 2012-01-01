(defun make-mapping (external-format)
   (loop FOR i FROM 1 BELOW char-code-limit
        FOR octets = (ignore-errors 
                       (string-to-octets (string (code-char i)) 
                                         :external-format external-format))
        WHEN octets
        COLLECT
        (progn (assert (null (or (position #xFF octets)
                                 (position #x00 octets)))
                       ()
                       "; ~a" octets)
               (list i octets))))

(defparameter *data* (make-mapping :cp932))

(defun array< (as bs)
  (loop FOR a ACROSS as
        FOR b ACROSS bs
    DO
    (cond ((< a b) (return-from array< t))
          ((> a b) (return-from array< nil)))
    FINALLY
    (return (< (length as) (length bs)))))

(defun uniq-mapping (data external-format)
  (loop WITH m = (make-hash-table :test #'equalp)
        FOR (code octets) IN data
    DO
    (if (null (gethash octets m))
        (setf (gethash octets m) code)
      (when (string= (string (code-char code))
                     (octets-to-string octets :external-format external-format))
        (setf (gethash octets m) code)))
    FINALLY
    (return (maphash-to-list (lambda (k v) (list v k)) m))))

(defparameter *data.uniq* (uniq-mapping *data*))

(defparameter *entries* (sort (mapcar (lambda (x) (reverse x)) *data.uniq*)
                              #'array< :key #'car))

(dabase:build *entries* "unicode-to-cp932.idx")
(defparameter *da* (dabase:load "unicode-to-cp932.idx"))

(defun read-uint (byte-width in)
  (loop FOR i FROM (1- byte-width) DOWNTO 0
        SUM (ash (read-byte in) (* i 8))))

(with-open-file (in "unicode-to-cp932.idx"
                    :element-type 'octet)
  (let ((count (read-uint 4 in)))
    (read-uint 4 in) ; eat element-count
    (defparameter *nodes* (loop REPEAT count COLLECT (read-uint 4 in)))))

(defun output-erlang (name nodes out)
  (format out "~&~a() ->~%" name)
  (format out "  {")
  (loop FOR i FROM 0
        FOR n IN nodes
    DO
    (when (plusp i)
      (format out ","))
    (when (zerop (mod i 10))
      (format out "~%   "))
    (format out "16#~8,'0x" n))
  (format out "~&  }.~%"))
(output-erlang "abc" '(1 2 3) *standard-output*)

(with-open-file (out (s #1="creole_from_cp932" ".erl") :direction :output :if-exists :supersede)
  (format out "-module(~a).~%" #1#)
  (format out "-export([da_nodes/0]).~2%")
  (output-erlang "da_nodes" *nodes* out)
  'done)

(defun output-erlang2 (name data out)
  (format out "~&~a(Code) ->~%" name)
  (format out "  case Code of~%")
  (loop FOR (code os) IN data
        FOR octets = (coerce os 'list)
        FOR i FROM 0
    DO
    (if (= 1 (length octets))
        (format out "~&    ~a -> ~a;" code (car octets))
      (format out "~&    ~a -> <<~{~a~^,~}>>;" code octets)))
  (format out "~&    _ -> fail")
  (format out "~&  end.~%"))

(output-erlang2 "abc" (subseq *data* 100 200) *standard-output*)

(with-open-file (out (s #1="creole_to_cp932" ".erl") :direction :output :if-exists :supersede)
  (format out "-module(~a).~%" #1#)
  (format out "-export([to_bytes/1]).~2%")
  (output-erlang2 "to_bytes" *data* out)
  'done)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *data* (make-mapping :euc-jp))
(defparameter *data.uniq* (uniq-mapping *data* :euc-jp))

(dabase:build (sort (mapcar (lambda (x) (reverse x)) *data.uniq*)
                    #'array< :key #'car)
              "unicode-to-eucjp.idx")

(defparameter *da* (dabase:load "unicode-to-eucjp.idx"))

(with-open-file (out (s #1="creole_to_eucjp" ".erl") :direction :output :if-exists :supersede)
  (format out "-module(~a).~%" #1#)
  (format out "-export([to_bytes/1]).~2%")
  (output-erlang2 "to_bytes" *data* out)
  'done)

(with-open-file (out (s #1="creole_from_eucjp" ".erl") :direction :output :if-exists :supersede)
  (format out "-module(~a).~%" #1#)
  (format out "-export([da_nodes/0]).~2%")
  (output-erlang "da_nodes" *nodes* out)
  'done)

(with-open-file (in "unicode-to-eucjp.idx"
                    :element-type 'octet)
  (let ((count (read-uint 4 in)))
    (read-uint 4 in) ; eat element-count
    (defparameter *nodes* (loop REPEAT count COLLECT (read-uint 4 in)))))


;;;;;
(defparameter *data* (make-mapping :sjis))
(defparameter *data.uniq* (uniq-mapping *data* :sjis))

(dabase:build (sort (mapcar (lambda (x) (reverse x)) *data.uniq*)
                    #'array< :key #'car)
              "unicode-to-sjis.idx")

(defparameter *da* (dabase:load "unicode-to-sjis.idx"))

(with-open-file (out (s #1="creole_to_sjis" ".erl") :direction :output :if-exists :supersede)
  (format out "-module(~a).~%" #1#)
  (format out "-export([to_bytes/1]).~2%")
  (output-erlang2 "to_bytes" *data* out)
  'done)

(with-open-file (in "unicode-to-sjis.idx"
                    :element-type 'octet)
  (let ((count (read-uint 4 in)))
    (read-uint 4 in) ; eat element-count
    (defparameter *nodes* (loop REPEAT count COLLECT (read-uint 4 in)))))

(with-open-file (out (s #1="creole_from_sjis" ".erl") :direction :output :if-exists :supersede)
  (format out "-module(~a).~%" #1#)
  (format out "-export([da_nodes/0]).~2%")
  (output-erlang "da_nodes" *nodes* out)
  'done)

;;;;; iso-2202-jp
(defun make-mapping2 ()
  (with-open-file (in "/home/ohta/dev/erlang/erl-creole/JIS_X_0208.txt")
    (loop FOR jis = (read in nil nil)
          FOR uni = (read in nil nil)
      WHILE jis
      COLLECT 
      (list uni (coerce (list (ldb (byte 8 8) jis)
                              (ldb (byte 8 0) jis))
                        'simple-octets)))))

(defparameter *data* (make-mapping2))

(defun uniq-count (list key)
  (let ((m (make-hash-table)))
    (dolist (x list (hash-table-count m))
      (incf (gethash (funcall key x) m 0)))))

(defparameter *data.uniq* *data*)

(dabase:build (sort (mapcar (lambda (x) (reverse x)) *data.uniq*)
                    #'array< :key #'car)
              "unicode-to-jis-x-0208-1990.idx")

(with-open-file (out (s #1="creole_to_jisx_0208_1990" ".erl") :direction :output :if-exists :supersede)
  (format out "-module(~a).~%" #1#)
  (format out "-export([to_bytes/1]).~2%")
  (output-erlang2 "to_bytes" *data* out)
  'done)

(with-open-file (in "unicode-to-jis-x-0208-1990.idx"
                    :element-type 'octet)
  (let ((count (read-uint 4 in)))
    (read-uint 4 in) ; eat element-count
    (defparameter *nodes* (loop REPEAT count COLLECT (read-uint 4 in)))))

(with-open-file (out (s #1="creole_from_jisx_0208_1990" ".erl") :direction :output :if-exists :supersede)
  (format out "-module(~a).~%" #1#)
  (format out "-export([da_nodes/0]).~2%")
  (output-erlang "da_nodes" *nodes* out)
  'done)

