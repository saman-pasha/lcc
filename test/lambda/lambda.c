#include <stdio.h>
#include <stdlib.h>
#include "lambda.h"
void Shape_staticDraw (Shape * this) {
  printf ("area from static draw method: %d\n", ((this ->length ) *  (this ->width ) ));
}
int (*aFunc) (int x, int y);
int mulFun (int x, int y) {
  return (x  *  y  );
}
typedef int (*twoIntInputFn_t) (int  , int  );
typedef typeof(aFunc ) otherWayDefFn_t;
int __lccLambda_main_103 (int x, int y) {
  return (x  +  y  );
}
int __lccLambda_main_106_108 (int x, int y) {
  return ((x  +  y  ) *  (x  +  y  ) );
}
int __lccLambda_main_106 (int x, int y) {
  { /* lcc#Let107 */
    __auto_type addFunVar = __lccLambda_main_106_108 ;
    return addFunVar (x , y );
  } /* lcc#Let107 */
}
void __lccLambda_main_112 (twoIntInputFn_t mathFunc, int a, int b) {
  printf ("from lambda as function pointer as parameter: %D\n", mathFunc (a , b ));
}
void __lccLambda_main_113 (Shape * shp) {
  printf ("Shape destructured\n");
}
int __lccLambda_main_114 (int m, int n) {
  return ((m  *  n  ) +  (m  *  n  ) );
}
void __lccLambda_main_115 (Shape * shp) {
  printf ("shape area is: %d\n", ((shp ->length ) *  (shp ->width ) ));
}
void __lccLambda_main_116 (Shape * shp) {
  printf ("shape env is: %d\n", (2 *  ((shp ->length ) +  (shp ->width ) ) ));
}
int main () {
  { /* lcc#Let102 */
    __auto_type addFunVar = __lccLambda_main_103 ;
    int (*addFunVar1) (int  , int  ) = __lccLambda_main_106;
    void (*doMath) (twoIntInputFn_t  , int  , int  ) = __lccLambda_main_112;
    int x = 4;
    int y = 5;
    Shape shp __attribute__((__cleanup__(__lccLambda_main_113 ))) = {10, 20};
    printf ("addition of x and y: %d\n", addFunVar (x , y ));
    aFunc  = mulFun ;
    printf ("product of x by y: %d\n", aFunc (x , y ));
    printf ("combo of x by y: %d\n", addFunVar1 (x , y ));
    doMath (__lccLambda_main_114 , 6, 7);
    (shp . dynamicDraw ) = __lccLambda_main_115 ;
    (shp . dynamicDraw )(&shp);
    Shape_staticDraw(&shp);
    __lccLambda_main_116 (&shp);
    return EXIT_SUCCESS ;
  } /* lcc#Let102 */
}
