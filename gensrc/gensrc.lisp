(require :asdf)

(asdf:load-system :dabase)

(deftype simple-octets () '(simple-array (unsigned-byte 8) (*)))
(defun to-bytes (code)
  (coerce
   (loop FOR i FROM (max 0 (1- (ceiling (integer-length code) 8))) DOWNTO 0
         COLLECT (ldb (byte 8 (* i 8)) code))
   'simple-octets))

(defun bytes< (as bs)
  (loop FOR a ACROSS as
        FOR b ACROSS bs
    DO
    (cond ((< a b) (return-from bytes< t))
          ((> a b) (return-from bytes< nil)))
    FINALLY
    (return (< (length as) (length bs)))))

(defun read-uint (byte-width in)
  (loop FOR i FROM (1- byte-width) DOWNTO 0
        SUM (ash (read-byte in) (* i 8))))

(defun read-mapping (path)
  (flet ((add-entry (map key val prio)
           (when (or (null (gethash key map))
                     (< prio (second (gethash key map))))
             (setf (gethash key map) (list val prio))))
         (to-list (map &aux acc)
           (maphash (lambda (k v)
                      (push (list k (first v)) acc))
                    map)
           acc))

    (with-open-file (in path)
      (loop WITH unicode->bytes = (make-hash-table)
            WITH bytes->unicode = (make-hash-table :test #'equalp)
            FOR unicode = (read in nil nil)
            WHILE unicode
            FOR bytes = (to-bytes (read in))
            FOR prio = (read in)
        DO
        (add-entry unicode->bytes unicode bytes prio)
        (add-entry bytes->unicode bytes unicode prio)
        FINALLY
        (return (values (sort (to-list unicode->bytes) #'< :key #'first)
                        (sort (to-list bytes->unicode) #'bytes< :key #'first)))))))

(defun gen-from-source (name bytes->uni)
  (dabase:build bytes->uni "uni2bytes.idx")
  (let ((nodes
         (unwind-protect
             (with-open-file (in "uni2bytes.idx" :element-type '(unsigned-byte 8))
               (let ((count (read-uint 4 in)))
                 (read-uint 4 in) ; eat element-count
                 (loop REPEAT count COLLECT (read-uint 4 in))))
           (delete-file "uni2bytes.idx")))
        (module (format nil "creole_from_~a" name)))

    (with-open-file (out (format nil "~a.erl" module) 
                         :direction :output :if-exists :supersede)
      (format out "-module(~a).~%" module)
      (format out "-export([da_nodes/0]).~2%")
      (format out "~&da_nodes() ->~%")
      (format out "  {")
      (loop FOR i FROM 0
            FOR n IN nodes
            DO
        (when (plusp i)
          (format out ","))
        (when (zerop (mod i 10))
          (format out "~%   "))
        (format out "16#~8,'0x" n))
      (format out "~&  }.~%"))))

(defun gen-to-source (name uni->bytes)
  (let ((module (format nil "creole_to_~a" name)))
    (with-open-file (out (format nil "~a.erl" module)
                         :direction :output :if-exists :supersede)
      (format out "-module(~a).~%" module)
      (format out "-export([to_bytes/1]).~2%")
      (format out "~&to_bytes(Code) ->~%")
      
      (format out "  case Code of~%")
      (loop FOR (code os) IN uni->bytes
            FOR octets = (coerce os 'list)
            FOR i FROM 0
        DO
        (if (= 1 (length octets))
            (format out "~&    ~a -> ~a;" code (car octets))
          (format out "~&    ~a -> <<~{~a~^,~}>>;" code octets)))
      (format out "~&    _ -> fail")
      (format out "~&  end.~%"))))

(setf *print-length* 300)
(defvar *encoding-name* (second sb-ext:*posix-argv*))
(defvar *mapping-file* (third sb-ext:*posix-argv*))

(multiple-value-bind (uni->bytes bytes->uni)
                     (read-mapping *mapping-file*)
  (gen-from-source *encoding-name* bytes->uni)
  (gen-to-source *encoding-name* uni->bytes)
  'done)

;; TODO: 512以下だとエラーになる
;;  (dabase:build (subseq *bytes->uni* 0 100) "uni2bytes.idx")