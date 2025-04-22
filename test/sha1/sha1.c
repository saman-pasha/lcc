#include <stdio.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include "sha.h"
#define SHA1_ROTL(bits, word) (((word) << (bits)) | ((word) >> (32-(bits)))
static uint32_t addTemp;
#define SHA1AddLength(context, length) \\
(addTemp = (context)->Length_Low, \\
(context)->Corrupted = \\
(((context)->Length_Low += (length)) < addTemp) && \\
(++(context)->Length_High == 0) ? shaInputTooLong \\
: (context)->Corrupted)
static void SHA1ProcessMessageBlock (SHA1Context * context);
static void SHA1Finalize (SHA1Context * context, uint8_t Pad_Byte);
static void SHA1PadMessage (SHA1Context * context, uint8_t Pad_Byte);
int SHA1Reset (SHA1Context * context) {
  if ((!context )) {
    return shaNull ;
  } 
  (context . Length_High ) = 0;
  (context ->Length_Low ) = 0;
  (context ->Message_Block_Index ) = 0;
  (context ->Intermediate_Hash )[0] = 0x67452301 ;
  (context ->Intermediate_Hash )[1] = 0xEFCDAB89 ;
  (context ->Intermediate_Hash )[2] = 0x98BADCFE ;
  (context ->Intermediate_Hash )[3] = 0x10325476 ;
  (context ->Intermediate_Hash )[4] = 0xC3D2E1F0 ;
  (context ->Computed ) = 0;
  (context ->Corrupted ) = shaSuccess ;
  return shaSuccess ;
}
int SHA1Input (SHA1Context * context, const uint8_t * message_array, unsigned length) {
  if ((!context )) {
    return shaNull ;
  } 
  if ((!length )) {
    return shaSuccess ;
  } 
  if ((!message_array )) {
    return shaNull ;
  } 
  if ((context ->Computed )) {
    { /* lcc#Block102 */
      (context ->Corrupted ) = shaStateError ;
      return shaStateError ;
    } /* lcc#Block102 */
  } 
  if ((context ->Corrupted )) {
    return (context ->Corrupted );
  } 
  while ((length --)) {
    (context ->Message_Block )[((context ->Message_Block_Index )++)] = contentof (message_array );
    if (((SHA1AddLength (context , 8) ==  shaSuccess  ) &&  ((context ->Message_Block_Index ) ==  SHA1_Message_Block_Size  ) )) {
      SHA1ProcessMessageBlock (context );
    } 
    (++message_array );
  } 
  return (context ->Corrupted );
}
int SHA1FinalBits (SHA1Context * context, uint8_t message_bits, unsigned int length) {
  if ((!context )) {
    return shaNull ;
  } 
  if ((!length )) {
    return shaSuccess ;
  } 
  if ((context ->Corrupted )) {
    return (context ->Corrupted );
  } 
  if ((context ->Computed )) {
    { /* lcc#Block103 */
      (context ->Corrupted ) = shaStateError ;
      return (context ->Corrupted );
    } /* lcc#Block103 */
  } 
  if ((length  >=  8 )) {
    { /* lcc#Block104 */
      (context ->Corrupted ) = shaBadParam ;
      return (context ->Corrupted );
    } /* lcc#Block104 */
  } 
  SHA1AddLength (context , length );
  { /* lcc#Let105 */
    static uint8_t masks[8] = {0x00 , 0x80 , 0xC0 , 0xE0 , 0xF0 , 0xF8 , 0xFC , 0xFE };
    static uint8_t markbit[8] = {0x80 , 0x40 , 0x20 , 0x10 , 0x08 , 0x04 , 0x02 , 0x01 };
    SHA1Finalize (context , ((uint8_t)((message_bits  &  masks [length ] ) |  markbit [length ] )));
  } /* lcc#Let105 */
  return (context ->Corrupted );
}
static void SHA224_256ProcessMessageBlock (SHA256Context * context) {
  { /* lcc#Let106 */
    static const uint32_t K[64] = {0x428a2f98 , 0x71374491 , 0xb5c0fbcf , 0xe9b5dba5 , 0x3956c25b , 0x59f111f1 , 0x923f82a4 , 0xab1c5ed5 , 0xd807aa98 , 0x12835b01 , 0x243185be , 0x550c7dc3 , 0x72be5d74 , 0x80deb1fe , 0x9bdc06a7 , 0xc19bf174 , 0xe49b69c1 , 0xefbe4786 , 0x0fc19dc6 , 0x240ca1cc , 0x2de92c6f , 0x4a7484aa , 0x5cb0a9dc , 0x76f988da , 0x983e5152 , 0xa831c66d , 0xb00327c8 , 0xbf597fc7 , 0xc6e00bf3 , 0xd5a79147 , 0x06ca6351 , 0x14292967 , 0x27b70a85 , 0x2e1b2138 , 0x4d2c6dfc , 0x53380d13 , 0x650a7354 , 0x766a0abb , 0x81c2c92e , 0x92722c85 , 0xa2bfe8a1 , 0xa81a664b , 0xc24b8b70 , 0xc76c51a3 , 0xd192e819 , 0xd6990624 , 0xf40e3585 , 0x106aa070 , 0x19a4c116 , 0x1e376c08 , 0x2748774c , 0x34b0bcb5 , 0x391c0cb3 , 0x4ed8aa4a , 0x5b9cca4f , 0x682e6ff3 , 0x748f82ee , 0x78a5636f , 0x84c87814 , 0x8cc70208 , 0x90befffa , 0xa4506ceb , 0xbef9a3f7 , 0xc67178f2 };
    static int t;
    static int t4;
    static uint32_t temp1;
    static uint32_t temp2;
    static uint32_t W[64];
    for ( t = 0,  t4 = 0; (t  <  16 );) {
      (t ++);
      t4  = (t4  +  4 );
      W [t ] = ((((uint32_t)(context ->Message_Block )[t4 ]) <<  24 ) |  (((uint32_t)(context ->Message_Block )[(t4  +  1 )]) <<  16 ) |  (((uint32_t)(context ->Message_Block )[(t4  +  2 )]) <<  8 ) |  ((uint32_t)(context ->Message_Block )[(t4  +  3 )]) );
    } 
    for ( t = 16; (t  <  64 );) {
      (t ++);
      W [t ] = (SHA256_sigma1 (W [(t  -  2 )]) +  W [(t  -  7 )] +  SHA256_sigma0 (W [(t  -  15 )]) +  W [(t  -  16 )] );
    } 
    { /* lcc#Let107 */
      uint32_t A;
      uint32_t B;
      uint32_t C;
      uint32_t D;
      uint32_t E;
      uint32_t F;
      uint32_t G;
      uint32_t H;
      A  = (context ->Intermediate_Hash )[0];
      B  = (context ->Intermediate_Hash )[1];
      C  = (context ->Intermediate_Hash )[2];
      D  = (context ->Intermediate_Hash )[3];
      E  = (context ->Intermediate_Hash )[4];
      F  = (context ->Intermediate_Hash )[5];
      G  = (context ->Intermediate_Hash )[6];
      H  = (context ->Intermediate_Hash )[7];
      for ( t = 0; (t  <  64 );) {
        (t ++);
        temp1  = (H  +  SHA256_SIGMA1 (E ) +  SHA_Ch (E , F , G ) +  K [t ] +  W [t ] );
        temp2  = (SHA256_SIGMA0 (A ) +  SHA_Maj (A , B , C ) );
        H  = G ;
        G  = F ;
        F  = E ;
        E  = (D  +  temp1  );
        D  = C ;
        C  = B ;
        B  = A ;
        A  = (temp1  +  temp2  );
      } 
      (context ->Intermediate_Hash )[0] +=  A  ;
      (context ->Intermediate_Hash )[1] +=  B  ;
      (context ->Intermediate_Hash )[2] +=  C  ;
      (context ->Intermediate_Hash )[3] +=  D  ;
      (context ->Intermediate_Hash )[4] +=  E  ;
      (context ->Intermediate_Hash )[5] +=  F  ;
      (context ->Intermediate_Hash )[6] +=  G  ;
      (context ->Intermediate_Hash )[7] +=  H  ;
    } /* lcc#Let107 */
  } /* lcc#Let106 */
  (context ->Message_Block_Index ) = 0;
}
