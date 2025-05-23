(in-package :cl-user)

(require "asdf")

;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(asdf:load-system "lcc")

(let ((argv (uiop:command-line-arguments)))
  (if (> (length argv) 0)
      (progn
        (loop for arg in argv
              with argc = (length argv)
              for i from 1 to argc
              when (> i 0)
              do (progn
                   (format t "arg specified: ~A~%" arg)
                   (cond
                     ((string= arg "--debug")    (setf lcc:*debug*   t))
                     ((string= arg "--warn")     (setf lcc:*warn*    t))
                     ((string= arg "--verbose")  (setf lcc:*verbose* "-v")))))
        (lcc:compile-lcc-file (first argv)))
      (error (format nil "at least pass the lcc .lisp file to compile"))))
