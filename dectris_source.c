//
// Dummy C external reader function
//
#include <stdint.h>
#include <stdio.h>

void generic_source_data ( int32_t image [])
{
  for (int i=0; i<=10; i++){ 
    image [i] = 0xdeadf00+i; // 233496320
  }
  return;
}

// struct header_point {
//
// }
void get_header ( int *nx, int *ny, int *nbyte, float *qx, float *qy, int *error_flag)// int *info_array[], int *error_flag)
{
  *nx       = 123;
  *ny       = 345;
  int depth = 32;

  *nbyte = depth * (*nx) * (*ny);
  printf(" [C] - get_header\n");
  printf("       + nx,ny      = < %d, %d >\n", *nx, *ny);
  printf("       + nbyte      = < %d >\n", *nbyte);

  *qx         = 0.;
  *qy         = 0.;
  *error_flag = 0;
  printf("       + qx,qy      = < %f, %f >\n", *qx, *qy);
  printf("       + error_flag = < %d > \n", *error_flag);
  
  return;
}

