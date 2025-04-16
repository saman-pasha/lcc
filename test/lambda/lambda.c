#include <stdio.h>
#include <stdlib.h>
int (*aFunc) (int x, int y);
int mulFun (int x, int y) {
  return (x  *  y  );
}
int __lccLambda_178 (int x, int y) {
  return (x  +  y  );
}
int __lccLambda_181 (int x, int y) {
  return ((x  +  y  ) *  (x  +  y  ) );
}
int __lccLambda_179 (int x, int y) {
  { /* lcc#Let180 */
    int (*addFunVar) (int x, int y) = __lccLambda_181;
    return addFunVar (x , y );
  } /* lcc#Let180 */
}
int main () {
  { /* lcc#Let177 */
    int (*addFunVar) (int x, int y) = __lccLambda_178;
    int (*addFunVar1) (int x, int y) = __lccLambda_179;
    int x = 4;
    int y = 5;
    printf ("addition of x and y: %d\n", addFunVar (x , y ));
    aFunc  = mulFun ;
    printf ("product of x by y: %d\n", aFunc (x , y ));
    printf ("combo of x by y: %d\n", addFunVar1 (x , y ));
    return EXIT_SUCCESS ;
  } /* lcc#Let177 */
}
