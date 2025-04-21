(in-package :lcc)

(format t "software type: ~S~%" (software-type))
;;;; os specific toolset
(defparameter *configs*
  (let ((os (software-type)))
    (cond 
      ((string= os "Linux") (list
                             'dumper   '("-Xclang" "-ast-dump")
                             'compiler '("libtool" "--tag=CC" "--mode=compile" "clang" "-g" "-O")
                             'linker   '("libtool" "--tag=CC" "--mode=link" "clang" "-g" "-O")))
      ((string= os "Darwin") (list
                              'dumper   '("-Xclang" "-ast-dump")
                              'compiler '("glibtool" "--tag=CC" "--mode=compile" "clang" "-g" "-O")
                              'linker   '("glibtool" "--tag=CC" "--mode=link" "clang" "-g" "-O")))
      (t (list
          'dumper   '("-Xclang" "-ast-dump")
          'compiler '("libtool" "--tag=CC" "--mode=compile" "clang" "-g" "-O")
          'linker   '("libtool" "--tag=CC" "--mode=link" "clang" "-g" "-O"))))))
