;;; not implemented
(source "generic.c" (:std #t :compile #t :link #t)

        (struct Containair [ T F ]
                (member T tt)
                (member F * ff))

        (func add [ T ] ((T x) (T y)) (out T)
              (return (+ x y)))

        (func main ()
              (printf "addition of x and y: %d\n" (add [ int ] 3 6))
              (return 0)))
