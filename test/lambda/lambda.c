#include <stdio.h>
#include <stdlib.h>
int (*aFunc) (int x, int y);
int mulFun (int x, int y) {
  return (x  *  y  );
}
typedef int (*twoIntInputFn_t) (int  , int  );
int __lccLambda_main_182 (int x, int y) {
  return (x  +  y  );
}
int __lccLambda_main_185_189 (int x, int y) {
  return ((x  +  y  ) *  (x  +  y  ) );
}
int __lccLambda_main_185 (int x, int y) {
  { /* lcc#Let186 */
    int (*addFunVar) (int  , int  ) = __lccLambda_main_185_189;
    return addFunVar (x , y );
  } /* lcc#Let186 */
}
void __lccLambda_main_193 (twoIntInputFn_t mathFunc, int a, int b) {
  printf ("from lambda as function pointer as parameter: %D\n", mathFunc (a , b ));
}
int __lccLambda_main_194 (int m, int n) {
  return ((m  *  n  ) +  (m  *  n  ) );
}
int main () {
  { /* lcc#Let179 */
    int (*addFunVar) (int  , int  ) = __lccLambda_main_182;
    int (*addFunVar1) (int  , int  ) = __lccLambda_main_185;
    void (*doMath) (twoIntInputFn_t  , int  , int  ) = __lccLambda_main_193;
    int x = 4;
    int y = 5;
    printf ("addition of x and y: %d\n", addFunVar (x , y ));
    aFunc  = mulFun ;
    printf ("product of x by y: %d\n", aFunc (x , y ));
    printf ("combo of x by y: %d\n", addFunVar1 (x , y ));
    doMath (__lccLambda_main_194 , 6, 7);
    return EXIT_SUCCESS ;
  } /* lcc#Let179 */
}
