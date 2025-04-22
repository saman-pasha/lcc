;;; defer for let variables destruction

(header "defer.h" ()
        (guard __EMP_H__
          (struct Employee
            (member int Id)
            (member char * Name))))

(source "defer.c" (:std #f
                   :compile #t
                   :link "-v -o defer_main -L{$CWD} -ldefer.o")
        (include <stdio.h> <stdlib.h> <string.h>)
        (include "defer.h")

        (func main ()
              (let ({defer '(lambda ((Employee ** empPtr))
                             (let ((Employee * emp . #'(cof empPtr)))
                               (printf "from defer, emp id is: %d and emp name is: %s\n" ($ emp Id) ($ emp Name))
                               (free ($ (cof empPtr) Name))
                               (free emp)
                               (printf "from defer, emp is freed\n")))}
                     (Employee * emp . #'(alloc (sizeof Employee)))
                     (Employee * empOther . #'(alloc (sizeof Employee))))

                (set ($ emp Id)   100
                     ($ emp Name) (calloc 8 (sizeof char)))

                (memcpy ($ emp Name) "Jon Doe\0" 8)
                
                (printf "emp id is: %d and emp name is: %s\n" ($ emp Id) ($ emp Name))

                )))
