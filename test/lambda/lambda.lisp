
(source "lambda.c" (:compile #t :link #t)
        (include <stdio.h>)
        
        (function main ()
                  (let ((auto #'* dou . '(lambda ((int x)) (return (* x x))))
                        (int y . 2))
                    (printf "double of y: %d\n" y)
                    ))
        )

