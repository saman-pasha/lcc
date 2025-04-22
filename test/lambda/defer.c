#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "defer.h"
void __lccLambda_main_101 (Employee ** empPtr) {
  { /* lcc#Let102 */
    Employee * emp = (*empPtr );
    printf ("from defer, emp id is: %d and emp name is: %s\n", (emp ->Id ), (emp ->Name ));
    free (((*empPtr )->Name ));
    free (emp );
    printf ("from defer, emp is freed\n");
  } /* lcc#Let102 */
}
void __lccLambda_main_103 (Employee ** empOther) {
  free ((*empOther ));
}
int main () {
  { /* lcc#Let100 */
    Employee * emp __attribute__((__cleanup__(__lccLambda_main_101 ))) = ((Employee *)malloc (sizeof(Employee)));
    Employee * empOther __attribute__((__cleanup__(__lccLambda_main_103 ))) = ((Employee *)malloc (sizeof(Employee)));
    (emp ->Id ) = 100;
    (emp ->Name ) = calloc (8, sizeof(char));
    memcpy ((emp ->Name ), "Jon Doe\0", 8);
    printf ("emp id is: %d and emp name is: %s\n", (emp ->Id ), (emp ->Name ));
  } /* lcc#Let100 */
}
