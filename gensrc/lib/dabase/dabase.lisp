(in-package :dabase)

(defun build (entries output-file)
  (dabase.builder:build entries output-file))

(defun load (index-file)
  (dabase.searcher:load index-file))

(defun member? (key da &key (start 0) (end (length key)))
  (dabase.searcher:member? key da start end))

(defun get (key da &key (start 0) (end (length key)))
  (dabase.searcher:get key da start end))

(defmacro each-common-prefix ((value position) (key da &key start end) &body body)
  `(dabase.searcher:each-common-prefix 
    (lambda (,value ,position)
      ,@body)
    ,key ,da ,(or start 0) ,(or end `(length ,key))))
