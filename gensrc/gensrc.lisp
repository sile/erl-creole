;;; ライブラリ読み込み
(require :asdf)
(asdf:load-system :dabase)


;;; 各種関数定義
(deftype simple-octets () '(simple-array (unsigned-byte 8) (*)))

;; 整数をバイト列(ビックエンディアン)に変換する
(defun to-bytes (code)
  (coerce
   (loop FOR i FROM (max 0 (1- (ceiling (integer-length code) 8))) DOWNTO 0
         COLLECT (ldb (byte 8 (* i 8)) code))
   'simple-octets))

;; バイト列の比較関数
(defun bytes< (as bs)
  (loop FOR a ACROSS as
        FOR b ACROSS bs
    DO
    (cond ((< a b) (return-from bytes< t))
          ((> a b) (return-from bytes< nil)))
    FINALLY
    (return (< (length as) (length bs)))))

;; 入力ストリームからNバイト整数を読み込む(ビッグエンディアン)
(defun read-uint (byte-width in)
  (loop FOR i FROM (1- byte-width) DOWNTO 0
        SUM (ash (read-byte in) (* i 8))))

;; マッピング(変換)定義ファイルを読み込む
;; => (values unicode->bytes:(list fixnum vector) bytes->unicode:(list vector fixnum))
(defun read-mapping (path)
  (flet ((add-entry (map key val prio)
           ;; キーがユニークになるようにチェックする (重複する場合は優先度の高い方を採用)
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

;; バイト列からユニコード文字列に変換するためのErlangモジュールを生成する
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

    (format t "~& generate: ~a~%" (format nil "~a.erl" module))
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

;; ユニコード文字列からバイト列に変換するためのErlangモジュールを生成する
(defun gen-to-source (name uni->bytes)
  (let ((module (format nil "creole_to_~a" name)))
    (format t "~& generate: ~a~%" (format nil "~a.erl" module))
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


;;; 引数取得
(when (/= (length sb-ext:*posix-argv*) 3)
  (format t "~& Usage: sbcl --script gensrc.lisp ENCODING_NAME MAPPING_DEF_FILE~2%")
  (sb-ext:quit))

(defvar *encoding-name* (second sb-ext:*posix-argv*))
(defvar *mapping-file* (third sb-ext:*posix-argv*))

;;; 実行
(multiple-value-bind (uni->bytes bytes->uni)
                     (read-mapping *mapping-file*)
  (gen-from-source *encoding-name* bytes->uni)
  (gen-to-source *encoding-name* uni->bytes)
  (format t "DONE~2%"))
