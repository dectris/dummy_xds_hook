//
// Copyright (C) 2016
// Dectris Ltd., Taefernweg 1, 5405 Baden-Daettwil, Switzerland.
// All rights reserved.
//
// vittorio.boccone@dectris.com
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


void get_header ( int *nx, int *ny, int *nbyte, float *qx, float *qy, 
		  int info_array[], int *error_flag)
{
  // Dummy values
  *nx       = 123;
  *ny       = 345;
  int depth = 32;

  *nbyte = depth * (*nx) * (*ny);
  printf(" [C] - get_header\n");
  printf("       + nx,ny         = < %d, %d >\n", *nx, *ny);
  printf("       + nbyte         = < %d >\n", *nbyte);

  *qx         = 0.;
  *qy         = 0.;
  *error_flag = 0;
  printf("       + qx,qy         = < %f, %f >\n", *qx, *qy);
  printf("       + error_flag    = < %d > \n", *error_flag);

  info_array[0] = 100;
  info_array[1] = 200;
  printf("       + array_info[0] = < %d >\n", info_array[0]);
  printf("       + array_info[1] = < %d >\n", info_array[1]);
 
  return;
}

