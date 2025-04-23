(defsystem "lcc"
  :version "0.0.7"
  :author  "Saman H. Pasha (saman.h.pasha@gmail.com)"
  :license "MIT License"
  :depends-on ("str" "cl-ppcre")
  :components ((:file "compiler"   :depends-on ("target"))
	           (:file "target"     :depends-on ("backend"))
	           (:file "backend"    :depends-on ("specifier"))
	           (:file "specifier"  :depends-on ("core"))
	           (:file "core"       :depends-on ("config"))
	           (:file "config"     :depends-on ("package"))
	           (:file "package"))
  :description "Lisp C Compiler aka. 'El-Cici' programming language, which compiles Lisp-like syntax to C code and more extra features like method, lambda, defer."
  :in-order-to ((test-op (test-op "test"))))
