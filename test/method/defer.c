#include <stdio.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include "method.h"
void Sample_PrintAttrA (Sample  * this) {
  printf("AttrA: %d\n", (this ->AttrA ));
}
void Sample_SetAttrA (Sample  * this, int a) {
  (this ->AttrA ) = a ;
}
void Sample_PrintAttrB (Sample  * this) {
  printf("AttrB: %s\n", (this ->AttrB ));
}
void Sample_SetAttrB (Sample  * this, char * b) {
  (this ->AttrB ) = b ;
}
void Sample_PrintBoth (Sample  * this) {
  Sample_PrintAttrA (this );
  Sample_PrintAttrB (this );
}
void callback_caller(void (**cbp)(void)) {
    (*cbp)();
}
int main () {
  { /* SCOPE178 */
    int * returned_pointer = NULL;
    
    Sample s = {100, "domain.com"};
    Sample * sPtr = ((Sample *)malloc(sizeof(Sample )));
    if (sPtr == NULL) printf("dynamic memory allocation failed! sPtr\n");
    Sample_PrintBoth (&s);
    int a = 1;
    returned_pointer = &(a); 
    goto return_main;

return_main:
    printf("will be cleaning %d and %s\n", s.AttrA, s.AttrB);
    free(sPtr);
    return (*returned_pointer);
  } /* SCOPE178 */
}
