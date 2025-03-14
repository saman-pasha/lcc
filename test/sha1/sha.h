#ifndef _SHA_H_
#define _SHA_H_
#ifndef _SHA_enum_
#define _SHA_enum_
enum { /* lcc#ENUM176 */
  shaSuccess = 0,
  shaNull,
  shaInputTooLong,
  shaStateError,
  shaBadParam
}; /* lcc#ENUM176 */
#endif /* _SHA_enum_ */ 
enum { /* lcc#ENUM177 */
  SHA1_Message_Block_Size = 64,
  SHA224_Message_Block_Size = 64,
  SHA256_Message_Block_Size = 64,
  SHA384_Message_Block_Size = 128,
  SHA512_Message_Block_Size = 128,
  USHA_Max_Message_Block_Size = SHA512_Message_Block_Size,
  SHA1HashSize = 20,
  SHA224HashSize = 28,
  SHA256HashSize = 32,
  SHA384HashSize = 48,
  SHA512HashSize = 64,
  USHAMaxHashSize = SHA512HashSize,
  SHA1HashSizeBits = 160,
  SHA224HashSizeBits = 224,
  SHA256HashSizeBits = 256,
  SHA384HashSizeBits = 384,
  SHA512HashSizeBits = 512,
  USHAMaxHashSizeBits = SHA512HashSizeBits
}; /* lcc#ENUM177 */
typedef enum SHAversion {
  SHA1,
  SHA224,
  SHA256,
  SHA384,
  SHA512
} SHAversion; /* SHAversion */
typedef struct SHA1Context {
  uint32_t Intermediate_Hash[(SHA1HashSize / 4)];
  uint32_t Length_High;
  uint32_t Length_Low;
  int_least16_t Message_Block_Index;
  uint8_t Message_Block[SHA1_Message_Block_Size];
  int Computed;
  int Corrupted;
} SHA1Context; /* SHA1Context */
typedef struct SHA256Context {
  uint32_t Intermediate_Hash[(SHA256HashSize / 4)];
  uint32_t Length_High;
  uint32_t Length_Low;
  int_least16_t Message_Block_Index;
  uint8_t Message_Block[SHA256_Message_Block_Size];
  int Computed;
  int Corrupted;
} SHA256Context; /* SHA256Context */
typedef struct SHA512Context {
#ifdefUSE_32BIT_ONLY
  uint32_t Intermediate_Hash32[(SHA512HashSize / 4)];
  uint32_t Length[4];
#else
  uint64_t Intermediate_Hash64[(SHA512HashSize / 8)];
  uint64_t Length_High;
  uint64_t Length_Low;
#endif
  int_least16_t Message_Block_Index;
  uint8_t Message_Block[SHA512_Message_Block_Size];
  int Computed;
  int Corrupted;
} SHA512Context; /* SHA512Context */
SHA256Context SHA224Contexttypedef T;
SHA512Context SHA384Contexttypedef T;
typedef struct USHAContext {
  int whichSha;
  union { /* lcc#UNION181 */
    SHA1Context sha1Context;
    SHA224Context sha224Context;
    SHA256Context sha256Context;
    SHA384Context sha384Context;
    SHA512Context sha512Context;
  } ctx; /* lcc#UNION181 */
} USHAContext; /* USHAContext */
extern int SHA1Reset (SHA1Context * /* lcc#PARAM182 */);
typedef int(*SHA1Reset_t) (SHA1Context * /* lcc#PARAM182 */);
extern int SHA1Input (unsigned int bytecount, const uint8_t * bytes, SHA1Context * /* lcc#PARAM183 */);
typedef int(*SHA1Input_t) (unsigned int bytecount, const uint8_t * bytes, SHA1Context * /* lcc#PARAM183 */);
extern int SHA1FinalBits (unsigned int bit_count, uint8_t bits, SHA1Context * /* lcc#PARAM184 */);
typedef int(*SHA1FinalBits_t) (unsigned int bit_count, uint8_t bits, SHA1Context * /* lcc#PARAM184 */);
extern int SHA1Result (uint8_t Message_Digest[SHA1HashSize], SHA1Context * /* lcc#PARAM185 */);
typedef int(*SHA1Result_t) (uint8_t Message_Digest[SHA1HashSize], SHA1Context * /* lcc#PARAM185 */);
extern int SHA224Reset (SHA224Context * /* lcc#PARAM186 */);
typedef int(*SHA224Reset_t) (SHA224Context * /* lcc#PARAM186 */);
extern int SHA224Input (unsigned int bytecount, const uint8_t * bytes, SHA224Context * /* lcc#PARAM187 */);
typedef int(*SHA224Input_t) (unsigned int bytecount, const uint8_t * bytes, SHA224Context * /* lcc#PARAM187 */);
extern int SHA224FinalBits (unsigned int bit_count, uint8_t bits, SHA224Context * /* lcc#PARAM188 */);
typedef int(*SHA224FinalBits_t) (unsigned int bit_count, uint8_t bits, SHA224Context * /* lcc#PARAM188 */);
extern int SHA224Result (uint8_t Message_Digest[SHA224HashSize], SHA224Context * /* lcc#PARAM189 */);
typedef int(*SHA224Result_t) (uint8_t Message_Digest[SHA224HashSize], SHA224Context * /* lcc#PARAM189 */);
extern int SHA256Reset (SHA256Context * /* lcc#PARAM190 */);
typedef int(*SHA256Reset_t) (SHA256Context * /* lcc#PARAM190 */);
extern int SHA256Input (unsigned int bytecount, const uint8_t * bytes, SHA256Context * /* lcc#PARAM191 */);
typedef int(*SHA256Input_t) (unsigned int bytecount, const uint8_t * bytes, SHA256Context * /* lcc#PARAM191 */);
extern int SHA256FinalBits (unsigned int bit_count, uint8_t bits, SHA256Context * /* lcc#PARAM192 */);
typedef int(*SHA256FinalBits_t) (unsigned int bit_count, uint8_t bits, SHA256Context * /* lcc#PARAM192 */);
extern int SHA256Result (uint8_t Message_Digest[SHA256HashSize], SHA256Context * /* lcc#PARAM193 */);
typedef int(*SHA256Result_t) (uint8_t Message_Digest[SHA256HashSize], SHA256Context * /* lcc#PARAM193 */);
extern int SHA384Reset (SHA384Context * /* lcc#PARAM194 */);
typedef int(*SHA384Reset_t) (SHA384Context * /* lcc#PARAM194 */);
extern int SHA384Input (unsigned int bytecount, const uint8_t * bytes, SHA384Context * /* lcc#PARAM195 */);
typedef int(*SHA384Input_t) (unsigned int bytecount, const uint8_t * bytes, SHA384Context * /* lcc#PARAM195 */);
extern int SHA384FinalBits (unsigned int bit_count, uint8_t bits, SHA384Context * /* lcc#PARAM196 */);
typedef int(*SHA384FinalBits_t) (unsigned int bit_count, uint8_t bits, SHA384Context * /* lcc#PARAM196 */);
extern int SHA384Result (uint8_t Message_Digest[SHA384HashSize], SHA384Context * /* lcc#PARAM197 */);
typedef int(*SHA384Result_t) (uint8_t Message_Digest[SHA384HashSize], SHA384Context * /* lcc#PARAM197 */);
extern int SHA512Reset (SHA512Context * /* lcc#PARAM198 */);
typedef int(*SHA512Reset_t) (SHA512Context * /* lcc#PARAM198 */);
extern int SHA512Input (unsigned int bytecount, const uint8_t * bytes, SHA512Context * /* lcc#PARAM199 */);
typedef int(*SHA512Input_t) (unsigned int bytecount, const uint8_t * bytes, SHA512Context * /* lcc#PARAM199 */);
extern int SHA512FinalBits (unsigned int bit_count, uint8_t bits, SHA512Context * /* lcc#PARAM200 */);
typedef int(*SHA512FinalBits_t) (unsigned int bit_count, uint8_t bits, SHA512Context * /* lcc#PARAM200 */);
extern int SHA512Result (uint8_t Message_Digest[SHA512HashSize], SHA512Context * /* lcc#PARAM201 */);
typedef int(*SHA512Result_t) (uint8_t Message_Digest[SHA512HashSize], SHA512Context * /* lcc#PARAM201 */);
extern int USHAReset (SHAversion whichSha, USHAContext * context);
typedef int(*USHAReset_t) (SHAversion whichSha, USHAContext * context);
extern int USHAInput (unsigned int bytecount, const uint8_t * bytes, USHAContext * context);
typedef int(*USHAInput_t) (unsigned int bytecount, const uint8_t * bytes, USHAContext * context);
extern int USHAFinalBits (unsigned int bit_count, uint8_t bits, USHAContext * context);
typedef int(*USHAFinalBits_t) (unsigned int bit_count, uint8_t bits, USHAContext * context);
extern int USHAResult (uint8_t Message_Digest[USHAMaxHashSize], USHAContext * context);
typedef int(*USHAResult_t) (uint8_t Message_Digest[USHAMaxHashSize], USHAContext * context);
extern int USHABlockSize (SHAversion whichSha);
typedef int(*USHABlockSize_t) (SHAversion whichSha);
extern int USHAHashSize (SHAversion whichSha);
typedef int(*USHAHashSize_t) (SHAversion whichSha);
extern int USHAHashSizeBits (SHAversion whichSha);
typedef int(*USHAHashSizeBits_t) (SHAversion whichSha);
extern const char * USHAHashName (SHAversion whichSha);
typedef const char *(*USHAHashName_t) (SHAversion whichSha);
#endif /* _SHA_H_ */ 
