#include <stdio.h>
#include <stdlib.h>
#include "basic.h"
void __lccLambda_main_101 (Employee ** emps_arr) {
  free ((*emps_arr ));
}
void __lccLambda_main_102 (Employee ** emps_ptr_arr) {
  free ((*emps_ptr_arr ));
}
int main (int argc, char * argv[]) {
  { /* lcc#Let100 */
    Employee emp1 = {1, "John Doe"};
    Employee emp2 = {.id = 1, .name = "John Doe"};
    Employee emp3 = {.id = 1, .name = "John Doe", .tag.tag_id = 1001};
    Employee emp_array[] = {{1, "John Doe"}, {2, "Saman Pasha"}};
    Employee * emp = ((Employee *)malloc (sizeof(Employee)));
    Employee * emps_arr __attribute__((__cleanup__(__lccLambda_main_101 ))) = ((Employee *)calloc (5, sizeof(Employee)));
    Employee * emps_ptr_arr __attribute__((__cleanup__(__lccLambda_main_102 ))) = ((Employee *)calloc (10, sizeof(Employee *)));
    free (emp );
    emp  = (&emp1 );
    printf ("sum of a series: %d\n", (1 +  2 +  3 +  4 +  5 ));
    printf ("is id one? %s\n", ((((emp1 . id ) ==  1 )) ? "true" : "false"));
    printf ("first emp: %s, second emp: %s\n", (emp_array [0]. name ), (emp_array [1]. name ));
    printf ("postfix 1+: %d, prefix ++: %d\n", ((emp ->id )++), (++(emp ->id )));
    (emp ->id ) +=  1 ;
    printf ("after assignment: %d\n", (emp1 . id ));
  } /* lcc#Let100 */
  { /* lcc#Block103 */
    printf ("Hi from inside of a block");
  } /* lcc#Block103 */
  return 0;
}
