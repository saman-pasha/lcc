#include <stdlib.h>
#include "method.h"
int main () {
  { /* lcc#Let100 */
    Sample s = {100, "domain.com"};
    Sample * sRef = (&s );
    Sample * sPtr = ((Sample *)malloc (sizeof(Sample)));
    Sample_PrintBoth(&s);
    Sample_SetAttrA(&s, 124);
    Sample_PrintBoth(sRef );
    return 0;
  } /* lcc#Let100 */
}
