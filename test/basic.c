#include <stdlib.h>
#include "basic.h"
int main (int argc, char * argv[]) {
  { /* lcc#Let100 */
    Employee emp1 = {1, "John Doe"};
    Employee emp2 = {.id = 1, .name = "John Doe"};
    Employee emp3 = {.id = 1, .name = "John Doe", .tag.tag_id = 1001};
    Employee emp_array[] = {{1, "John Doe"}, {2, "Saman Pasha"}};
    Employee * emp = ((Employee *)malloc (sizeof(Employee)));
    Employee * emps_arr = ((Employee *)calloc (5, sizeof(Employee)));
    Employee * emps_ptr_arr = ((Employee *)calloc (10, sizeof(Employee *)));
    free (emp );
    emp  = (&emp1 );
    printf ("sum of a series: %d\n", (1 +  2 +  3 +  4 +  5 ));
    printf ("is id one? %s\n", ((((emp1 . id ) ==  1 )) ? "true" : "false"));
    printf ("first emp: %s, second emp: %s\n", (emp_array [0]. name ), (emp_array [1]. name ));
    printf ("postfix ++#: %d, prefix ++: %d\n", ((emp ->id )++), (++(emp ->id )));
    (emp ->id ) +=  1 ;
    printf ("after assignment: %d\n", (emp1 . id ));
  } /* lcc#Let100 */
  { /* lcc#Block101 */
    printf ("Hi from inside of a block");
  } /* lcc#Block101 */
  1;
  return 0;
}
