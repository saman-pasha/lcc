
(source "lambda.c" (:std #f
                         :compile "-c lambda.c -o lambda_main.o"
                         :link "-v -o lambda_main -L{$CWD} -llambda_main.o -L{$CWD}../method -lmethod.o")
        (include <stdio.h>)
        (include "../method/method.h")

        (variable function aFunc ((int x) (int y)) (returns int))

        (function mul ((int x) (int y)) (returns int)
                  (return (* x y)))
        
        (function main ()
                  (let ((function funVar ((int x) (int y)) (returns int) .
                                  '(lambda ((int x) (int y)) (returns int)
                                    (return (+ x y))))
                        (int y . 2)
                        (Sample s))
                    (set ($ s AttrA) 12)
                    (set mulVar mul)
                    
                    (printf "product of x by y: %d\n" (mulVar 33 y))
                    ))
        )
