#include <stdio.h>
#include <stdlib.h>
int (*aFunc) (int y, int x);
int mulFun (int y, int x) {
  return (x * y );
}
int main () {
  { /* LET SCOPE */
    int (*addFunVar) (int y, int x) = __lccLambda_100 ;
    int (*addFunVar1) (int y, int x) = __lccLambda_101 ;
    int x = 2;
    int y = 3;
    printf("addition of x by y: %d\n", addFunVar(x , y ));
    x  = 4;
    y  = 5;
    aFunc  = mulFun ;
    printf("product of x by y: %d\n", aFunc(x , y ));
    return EXIT_SUCCESS ;
  } /* LET SCOPE */
}
