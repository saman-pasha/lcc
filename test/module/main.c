#include <stdio.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include "module.h"
int main () {
  { /* lcc#Let100 */
    int n = 3;
    int t = 4;
    __auto_type mra = lccTozMYGkJ8oVe_3afFeftV_4FBR8_ (n , t );
    __auto_type mrb = lccSCZ69Ii4twzs6m1xrpUrU_Y2Jjk_ (n , t );
    __auto_type mrc = lcctoldQFn2V3YHvs0kVlusT3lYqCs_ (n , t );
    __auto_type mrd = lcc7hMT88tey7fEi_6cHAML4lQ3Vg4_ (n , t );
    Employee fEmp = {10, "Cicili"};
    lccxqUhX6O6k74k_WEKLfDVc2Xgz20_ aEmp = {11, "Jon Doe"};
    lccEHa_SpDFo5BFIAK1zxc3Gc8SVdg_ dEmp = {12, "Saman Pasha"};
    printf ("a module a: %d, b: %d\n", (mra . a ), (mra . b ));
    printf ("b module a: %d, b: %d\n", (mrb . a ), (mrb . b ));
    printf ("c module a: %d, b: %d\n", (mrc . a ), (mrc . b ));
    printf ("d module a: %d, b: %d\n", (mrd . a ), (mrd . b ));
    Employee_print1(&fEmp, ((Employee *)(&aEmp )));
    Employee_print2(&fEmp, ((Employee *)(&dEmp )));
    lccxqUhX6O6k74k_WEKLfDVc2Xgz20__print(&aEmp);
    lccEHa_SpDFo5BFIAK1zxc3Gc8SVdg__print(&dEmp);
    printf ("free varAAA: %ld, inside c: %ld\n", varAAA , lccC41hdVPk87iu_w5AQBJtphUnZF8_ );
    printf ("free Circle: %d, inside b: %d\n", Circle , lccPPaHIKkHcWl6aoepLUAVTFt6PME_ );
    printf ("free Rectangle: %d, inside b: %d\n", Rectangle , lccjGltd46DCpqLS00SKeqdxOid77o_ );
  } /* lcc#Let100 */
}
