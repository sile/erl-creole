(defpackage dabase
  (:use :common-lisp)
  (:shadow :common-lisp get load)
  (:export build
           load
           element-count
           member?
           get
           each-common-prefix))
(in-package :dabase)
