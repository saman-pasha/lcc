;;; demonstrating function variable and lambda
(source "lambda.c" (:std #f
                         :compile "-c lambda.c -o lambda_main.o"
                         :link "-v -o lambda_main -L{$CWD} -llambda_main.o")
        (include <stdio.h>)
        (include <stdlib.h>)

        (variable function aFunc ((int x) (int y)) (returns int))

        (function mulFun ((int x) (int y)) (returns int)
                  (return (* x y)))
        
        (function main ()
                  (let ((function addFunVar ((int x) (int y)) (returns int) . 
                                  '(lambda ((int x) (int y)) (returns int)
                                    (return (+ x y))))
                        (function addFunVar1 ((int x) (int y)) (returns int) . 
                                  '(lambda ((int x) (int y)) (returns int)
                                    (let ((function addFunVar ((int x) (int y)) (returns int) . 
                                                    '(lambda ((int x) (int y)) (returns int)
                                                      (return (+ (+ x y) (+ x y)))))))
                                    (return (+ x y))))
                        (int x . 2)
                        (int y . 3))
                    
                    (printf "addition of x by y: %d\n" (addFunVar x y))

                    (set x 4 y 5 aFunc mulFun)
                    (printf "product of x by y: %d\n" (aFunc x y))
                    (return EXIT_SUCCESS)
                    )))
