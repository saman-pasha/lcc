#include <stdio.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include "method.h"
int main () {
  { /* SCOPE179 */
    Sample s = {100, "domain.com"};
    Sample * sRef = (&s );
    Sample * sPtr = ((Sample *)malloc(sizeof(Sample )));
    if (sPtr == NULL) printf("dynamic memory allocation failed! sPtr\n");
    Sample_PrintBoth(&s);
    Sample_SetAttrA(&s, 124);
    Sample_PrintBoth(sRef );
    return 0;
    free(sPtr);
  } /* SCOPE179 */
}
