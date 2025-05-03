#include <stdlib.h>
#include "method.h"
void __lccLambda_main_101 (Sample ** sPtr) {
  free ((*sPtr ));
}
int main () {
  { /* lcc#Let100 */
    Sample s = {100, "domain.com"};
    Sample * sRef = (&s );
    Sample * sPtr __attribute__((__cleanup__(__lccLambda_main_101 ))) = ((Sample *)malloc (sizeof(Sample)));
    Sample_PrintBoth(&s);
    Sample_SetAttrA(&s, 124);
    Sample_PrintBoth(sRef );
    return 0;
  } /* lcc#Let100 */
}
