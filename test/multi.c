#include <stdio.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
typedef struct MultiReturn_t {
  int a;
  int b;
} MultiReturn_t;
typedef struct __lccStruct_aMultiReturnFunc_177 {
  int a;
  int b;
} __lccStruct_aMultiReturnFunc_177;
__lccStruct_aMultiReturnFunc_177 aMultiReturnFunc (int x, int y) {
  return ((__lccStruct_aMultiReturnFunc_177){x , y });
}
typedef struct __lccStruct_aMultiReturnFuncPtr_178 {
  int a;
  int b;
} __lccStruct_aMultiReturnFuncPtr_178;
__lccStruct_aMultiReturnFuncPtr_178 * aMultiReturnFuncPtr (int x, int y) {
  { /* lcc#Let179 */
    typeof(aMultiReturnFuncPtr (x , y )) output = ((typeof(aMultiReturnFuncPtr (x , y )))malloc (sizeof(typeof((*aMultiReturnFuncPtr (x , y ))))));
    (*output ) = ((typeof((*aMultiReturnFuncPtr (x , y )))){x , y });
    return ((__lccStruct_aMultiReturnFuncPtr_178 *)output );
  } /* lcc#Let179 */
}
MultiReturn_t aMultiReturnFuncKV (int x, int y) {
  return ((MultiReturn_t){.a = x , .b = y });
}
typedef struct __lccStruct_aMultiReturnFuncS_180 {
  int a;
  int b;
} __lccStruct_aMultiReturnFuncS_180;
__lccStruct_aMultiReturnFuncS_180 aMultiReturnFuncS (int x, int y) {
  { /* lcc#Let181 */
    typeof(aMultiReturnFuncS (x , y )) s = {x , y };
    return ((__lccStruct_aMultiReturnFuncS_180)s );
  } /* lcc#Let181 */
}
int main () {
  { /* lcc#Let182 */
    int n = 3;
    int t = 4;
    MultiReturn_t mrt;
    typeof(aMultiReturnFunc (1, 1)) mr;
    typeof(aMultiReturnFuncPtr (1, 1)) mrtPtr;
    mr  = aMultiReturnFunc (n , t );
    printf ("a: %d, b: %d\n", (mr . a ), (mr . b ));
    mrt  = aMultiReturnFuncKV ((++n ), (++t ));
    printf ("a: %d, b: %d\n", (mrt . a ), (mrt . b ));
    mrtPtr  = aMultiReturnFuncPtr ((++n ), (++t ));
    printf ("a: %d, b: %d\n", (mrtPtr ->a ), (mrtPtr ->b ));
  } /* lcc#Let182 */
}
