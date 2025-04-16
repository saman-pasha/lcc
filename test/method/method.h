#ifndef __SAMPLE_H__
#define __SAMPLE_H__
typedef struct Sample {
  int AttrA;
  char * AttrB;
} Sample;
void Sample_PrintAttrA (Sample * this);
void Sample_SetAttrA (Sample * this, int a);
void Sample_PrintAttrB (Sample * this);
void Sample_SetAttrB (Sample * this, char * b);
void Sample_PrintBoth (Sample * this);
__attribute__((weak)) void Sample_WithoutResolver (Sample * this) {
  (this ->AttrA ) = 12;
  ((*this ). AttrB ) = "Saman";
  Sample_PrintBoth (this );
}
#endif /* __SAMPLE_H__ */ 
