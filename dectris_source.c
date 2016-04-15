/*
  Copyright (C) 2016
  Dectris Ltd., Taefernweg 1, 5405 Baden-Daettwil, Switzerland.
  All rights reserved.

  vittorio.boccone@dectris.com
 
  Dummy C external reader function
*/

#include <stdint.h>
#include <stdio.h>

/*    Arguments:
      'frame_number' (*int)  input number of pixels along x 
      'nx' (*int)            input number of pixels along x 
      'ny' (*int)            input number of pixels along y
      'data_array' (int [])  output image 
      'error_flag' (*int)    output  Provides error state condition
                                      0 Success
                                     -1 Library not loaded (provided on the fortran side)
                                     -2 Cannot read frame
*/
void get_data (int *frame_number, int *nx, int *ny, int *data_array[] , int *error_flag)
{

  printf(" [C] - get_data\n");
  printf("       + nx,ny         = < %d, %d >\n", *nx, *ny);
  //printf("       + len(array) = < %d >\n", (int)(sizeof(iptr)));


  // return;

  // printf("       + len(array) = < %d >\n", (int)(sizeof(data_array)/sizeof(data_array[0])));
  // printf("       + len(array) = < %d >\n", (int)(sizeof(data_array[0])/sizeof(data_array[0][0])));

  for (int i=0; i<*ny; i++){
      for (int j=0; j<*nx; j++){
	printf("data_array[%d][%d]=%d\n",i,j,data_array[j+(*ny)*i]);
      }
  }
  // data_array[0][0] = 112;
  //for (int i=0; i<=*nx; i++){ 
  //  for (int j = 0; j <= *ny;j++){
  //    iptr[i+((*nx)*j)] = i+j; // 233496320
  //  }
  // }
  *error_flag = 0;

  return;
}


/*    Arguments:
      'nx' (*int)            output  number of pixels along x 
      'ny' (*int)            output  number of pixels along y
      'nbyte' (*int)         output  number of bytes in the image... x*y*depth
      'qx' (float)           output  pixel size
      'qy' (float)           output  pixel size
      'info' (int [])        output  array of (1024) integers:
                                      - info(0)       = Dectris 
                                      - info(1)       = version number of the library
                                      - info(2..1023) = unused
      'error_flag' (integer) output  Provides error state condition
                                      0 Success
                                     -1 Library not loaded (provided on the fortran side)
                                     -2 Cannot read header
*/
void get_header ( int *nx, int *ny, int *nbyte, float *qx, float *qy, 
		  int info_array[], int *error_flag)
{

  // Dummy values
  *nx       = 10;
  *ny       = 5;
  int depth = 32;

  *nbyte = depth * (*nx) * (*ny);
  printf(" [C] - get_header\n");
  printf("       + nx,ny         = < %d, %d >\n", *nx, *ny);
  printf("       + nbyte         = < %d >\n", *nbyte);

  *qx         = 75.e-6;
  *qy         = 75.e-6;
  *error_flag = 0;
  printf("       + qx,qy         = < %f, %f >\n", *qx, *qy);
  printf("       + error_flag    = < %d > \n", *error_flag);

  info_array[0] = 100;
  info_array[1] = 200;
  printf("       + array_info[0] = < %d >\n", info_array[0]);
  printf("       + array_info[1] = < %d >\n", info_array[1]);
 
  return;
}

