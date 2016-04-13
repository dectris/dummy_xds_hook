//
// dummy external image provider v0.0.1
//
#include <stdint.h>

void external_image_read ( int32_t image [])
{
  for (int i=0; i<=10; i++){ 
    image [i] = 0xdeadf00+i; // 233496320
  }
  return;
}
