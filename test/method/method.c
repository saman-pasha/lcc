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
  Sample_PrintAttrA(this );
  Sample_PrintAttrB(this );
}
