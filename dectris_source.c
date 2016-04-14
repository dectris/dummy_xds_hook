//
// Dummy C external reader function
//
#include <stdint.h>

void generic_source_data ( int32_t image [])
{
  for (int i=0; i<=10; i++){ 
    image [i] = 0xdeadf00+i; // 233496320
  }
  return;
}

