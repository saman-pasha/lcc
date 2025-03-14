;;; methods and receivers

;;;; declaration of sample struct
(header "method.h" ()

        (guard __SAMPLE_H__

          ;; sample struct representing members and methods
          (struct Sample
            (member int AttrA)
            (member char * AttrB))

          ;; receivers of Sample struct
          ;; functions which will be defined in source file could use resolver
          {declare} (method Sample->PrintAttrA ())
          {declare} (method Sample->SetAttrA ((int a)))
          {declare} (method Sample->PrintAttrB ())
          {declare} (method Sample->SetAttrB ((char * b)))
          {declare} (method Sample->PrintBoth ())

          ;; functions which defined inside header file couldn't use resolver
          {inline} (method Sample->WithoutResolver ()
                           (set (-> this AttrA) 12)
                           (set ($ (contentof this) AttrB) "Saman")
                           (Sample_PrintBoth this))
          
        ))

;;;; definition of sample struct methods
(source "method.c" (:std #t :compile #t :link #t)
        (include "method.h")

        ;; methods of Sample struct which access to Sample members and other methods

        (method Sample->PrintAttrA ()
                (printf "AttrA: %d\n" ($ this AttrA)))

        (method Sample->SetAttrA ((int a))
                (set ($ this AttrA) a))
        
        (method Sample->PrintAttrB ()
                (printf "AttrB: %s\n" ($ this AttrB)))

        (method Sample->SetAttrB ((char * b))
                (set ($ this AttrB) b))
        
        (method Sample->PrintBoth ()
                (-> this PrintAttrA)
                (-> this PrintAttrB))

        (function main () (returns int)
                  (let ((Sample s . '{ 100 "domain.com" })
                        (Sample * sPtr . #'(alloc (sizeof Sample))))
                    (-> s PrintBoth)
                    (return 0)))
        )
