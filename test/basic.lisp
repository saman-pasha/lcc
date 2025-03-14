(in-package :cl-user)

(require "asdf")

;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                        (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(asdf:load-system "lcc")

(in-package :lcc)

(defvar globals (make-hash-table :test 'eql))

(assert (specify-variable '(|variable| |long| |x|) '() 0 globals) nil "(variable long x)")
(assert (specify-variable '(|variable| |long| |var2| . 12) '() 0 globals) nil "(variable long var2 . 12)")
(assert (specify-variable '(|variable| |long| |arr| []. '{1 2}) '() 0 globals) nil "(variable long arr [] . '{1 2})")
(assert (specify-variable '(|variable| |long| |var3| . 24) '({|static|}) 0 globals) nil "{static} (variable long var3 . 24)")

(assert (string= (compile-type< '(|long|) globals) "long") nil "long")
(assert (string= (compile-type< '(|const| |long|) globals) "const long") nil "const long")
(assert (string= (compile-type< '(|long| *) globals) "long *") nil "long *")
(assert (string= (compile-type< '(|long| &) globals) "long &") nil "long &")
(assert (string= (compile-type< '(|long| []) globals) "long []") nil "long []")
(assert (string= (compile-type< '(|long| |x|) globals) "long x") nil "long x")

(assert (string= (compile-type< '(|const| |long| *) globals) "const long *") nil "const long *")
(assert (string= (compile-type< '(|const| |long| &) globals) "const long &") nil "const long &")
(assert (string= (compile-type< '(|const| |long| []) globals) "const long []") nil "const long []")
(assert (string= (compile-type< '(|const| |long| |x|) globals) "const long x") nil "const long x")
(assert (string= (compile-type< '(|long| * |const|) globals) "long * const") nil "long * const")
(assert (string= (compile-type< '(|long| & []) globals) "long & []") nil "long & []")
(assert (string= (compile-type< '(|long| & |x|) globals) "long & x") nil "long & x")
(assert (string= (compile-type< '(|long| |x| []) globals) "long x []") nil "long x []")

(assert (string= (compile-type< '(|const| |long| * |const|) globals) "const long * const") nil "const long * const")
(assert (string= (compile-type< '(|const| |long| & []) globals) "const long & []") nil "const long & []")
(assert (string= (compile-type< '(|const| |long| & |x|) globals) "const long & x") nil "const long & x")
(assert (string= (compile-type< '(|const| |long| |x| []) globals) "const long x []") nil "const long x []")
(assert (string= (compile-type< '(|long| * |const| []) globals) "long * const []") nil "long * const []")
(assert (string= (compile-type< '(|long| * |const| |x|) globals) "long * const x") nil "long * const x")
(assert (string= (compile-type< '(|long| & |x| []) globals) "long & x []") nil "long & x []")

(assert (string= (compile-type< '(|const| |long| * |const| []) globals) "const long * const []") nil "const long * const []")
(assert (string= (compile-type< '(|const| |long| * |const| |x|) globals) "const long * const x") nil "const long * const x")

(assert (string= (compile-type< '(|const| |long| * |const| |x| []) globals) "const long * const x []") nil "const long * const x []")

(assert (string= (compile-operator< '(* 1 2 3) globals) "(1 * 2 * 3)") nil "(1 * 2 * 3)")

(assert (string= (compile-unary< '(! 1) globals) "!1") nil "!1")
(assert (string= (compile-unary< '(|++#| |x|) globals) "x++") nil "x++")

(assert (string= (compile-form< '(|?| (> 2 1) (* |x| 2) (/ |x| 2)) globals) "(((2 > 1)) ? (x * 2) : (x / 2))") nil "(((2 > 1)) ? (x * 2) : (x / 2))")
(assert (string= (compile-form< '(|cast| |uint| (* 2 2)) globals) "((unsigned int)(2 * 2))") nil "((unsigned int)((2 * 2)))")

(assert (specify-function '(|function| |square| ((|int|)) (|returns| |int|)) '({|declare|}) 0 globals)
  nil
  "{declare} (function square ((int)) (returns int))")
(assert (specify-function '(|function| |sum| ((|long| |x|) (|long| |y| . 2)) (-> |long|) (|return| (+ |x| |y|))) '() 0 globals)
  nil
  "(function sum ((long x) (long y . 2)) (-> long) (return (+ x y)))")
(assert (specify-function '(|function| |square| ((|int| |x|)) (|->| |int|) (|return| (* |x| |x|))) '({|inline|} {|static|}) 0 globals)
  nil
  "{inline} {static} (function square ((int x)) (-> int) (return (* x x)))")
(assert (specify-function '(|function| |main| ((|int| |argc|) (|char| ** |argv|)) (|return| 0)) '() 0 globals)
  nil
  "(function main ((int argc) (char ** argv)) (return 0))")

;; (display (specify-preprocessor '(|@define| (|code| "HW \"Hello World!\"")) '() 0 globals))
;; (assert (specify-preprocessor '(|@define| (|code| "HW \"Hello World!\"")) '() 0 globals) nil "(@define $\"HW \"Hello World!\"\"$)")
;; (assert (spefify-preprocessor '(|@define| (|code| "SQUARE (x) x * x")) '() 0 globals) nil "(@define $\"SQUARE (x) x * x\"S)")

(assert (specify-enum '(|enum| (CONST1 . 0) (CONST2)) '() 0 globals) nil "(enum (CONST1 . 0) (CONST2))")
(assert (specify-enum '(|enum| |STATES| (STATE1) (STATE2)) '() 0 globals) nil "(enum STATES (STATE1) (STATE2))")

(assert (specify-guard '(|guard| _GUARD_H_) '() 0 globals) nil "(guard _GUARD_H_)")
