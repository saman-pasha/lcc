(in-package :cl-user)

(defpackage :lcc
  (:use :cl)
  (:export
    :*debug*
    :*warn*
    :*verbose*
    :compile-ast
    :compile-lcc-file))
