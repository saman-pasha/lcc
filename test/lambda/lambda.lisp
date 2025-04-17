;;; demonstrating function variable and lambda
(source "lambda.c" (:std #f
                    :compile #t
                    :link "-v -o main -L{$CWD} -llambda.o")
        (include <stdio.h>)
        (include <stdlib.h>)

        (var func aFunc ((int x) (int y)) (out int))

        (func mulFun ((int x) (int y)) (out int)
                  (return (* x y)))

        (typedef func twoIntInputFn_t ((int _) (int _)) (out int))
        
        (func main ()
                  (let ((func addFunVar ((int _) (int _)) (out int) . 
                              '(lambda ((int x) (int y)) (out int)
                                (return (+ x y))))
                        
                        (func addFunVar1 ((int _) (int _)) (out int) . 
                              '(lambda ((int x) (int y)) (out int)
                                (let ((func addFunVar ((int _) (int _)) (out int) . 
                                            '(lambda ((int x) (int y)) (out int)
                                              (return (* (+ x y) (+ x y))))))
                                  (return (addFunVar x y)))))

                        (func doMath ((twoIntInputFn_t _) (int _) (int _)) .
                              '(lambda ((twoIntInputFn_t mathFunc) (int a) (int b))
                                (printf "from lambda as function pointer as parameter: %D\n" (mathFunc a b))))
                        
                        (int x . 4)
                        (int y . 5))
                    
                    (printf "addition of x and y: %d\n" (addFunVar x y))

                    (set aFunc mulFun)
                    (printf "product of x by y: %d\n" (aFunc x y))

                    (printf "combo of x by y: %d\n" (addFunVar1 x y))

                    (doMath '(lambda ((int m) (int n)) (out int)
                              (return (+ (* m n) (* m n))))
                      6 7)
                    
                    (return EXIT_SUCCESS)
                    )))
