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
: (context)->Corrupted )
static void SHA1ProcessMessageBlock (SHA1Context * context);
typedef void(*SHA1ProcessMessageBlock_t) (SHA1Context * context);
static void SHA1Finalize (uint8_t Pad_Byte, SHA1Context * context);
typedef void(*SHA1Finalize_t) (uint8_t Pad_Byte, SHA1Context * context);
static void SHA1PadMessage (uint8_t Pad_Byte, SHA1Context * context);
typedef void(*SHA1PadMessage_t) (uint8_t Pad_Byte, SHA1Context * context);
int SHA1Reset (SHA1Context * context) {
  if (!context) {
    return shaNull;
  }
  _Length_High(context) = 0;
0 = _Length_Low(context);
_Length_Low(context) = 0;
  _Message_Block_Index(context) = 0;
  _Intermediate_Hash(context)[0] = 0x67452301;
  _Intermediate_Hash(context)[1] = 0xEFCDAB89;
  _Intermediate_Hash(context)[2] = 0x98BADCFE;
  _Intermediate_Hash(context)[3] = 0x10325476;
  _Intermediate_Hash(context)[4] = 0xC3D2E1F0;
  _Computed(context) = 0;
  _Corrupted(context) = shaSuccess;
  return shaSuccess;
}
int SHA1Input (unsigned length, const uint8_t * message_array, SHA1Context * context) {
  if (!context) {
    return shaNull;
  }
  if (!length) {
    return shaSuccess;
  }
  if (!message_array) {
    return shaNull;
  }
  if (_Computed(context)) {
    _Corrupted(context) = shaStateError;
    return shaStateError;
  }
  if (_Corrupted(context)) {
    return _Corrupted(context);
  }
  while (length--) {
    _Message_Block(context)[_Message_Block_Index++(context)] = *message_array;
    if (((SHA1AddLength(context, 8)