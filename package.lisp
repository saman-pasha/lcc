(in-package :cl-user)

(defpackage :lcc
  (:use :cl)
  (:export
    :*debug*
    :*warn*
    :compile-ast
    :compile-lcc-file))
