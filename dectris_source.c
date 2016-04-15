/*
  Copyright (C) 2016
  Dectris Ltd., Taefernweg 1, 5405 Baden-Daettwil, Switzerland.
  All rights reserved.

  vittorio.boccone@dectris.com
 
  Dummy C external reader function
*/

#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

char *hdf5_master_file_name="";


void hdf5_open(char *hdf5_filename, int *error_flag){
  /*    Arguments:
	'hdf5_filename' (*char) input   Name of the HDF5 master file
	'error_flag' (*int)     output  Provides error state condition
                                         0 Success
                                        -4 Error opening HDF5 master file
  */
  
  printf(" [C] - hdf5_open '%s'\n", hdf5_filename );
  hdf5_master_file_name = hdf5_filename;

  *error_flag = 0;
  // *error_flag = -4; // Masterfile cannot be opened
  return;
}


void hdf5_close(int *error_flag){
  /*    Arguments:
	'error_flag' (*int)     output  Provides error state condition
                                         0 Success
                                        -4 Error closing HDF5 master file
  */ 
  printf(" [C] - hdf5_close %s\n", hdf5_master_file_name);

  *error_flag = 0;
  // *error_flag = -4; // Masterfile cannot be closed
  return;
}


void get_data (int *frame_number, int *nx, int *ny, int data_array[], int *error_flag){
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

  printf(" [C] - get_data\n");
  printf("       + nx,ny         = < %d, %d >\n", *nx, *ny);

  // Fill in dummy values
  int old_data;
  for (int i=0; i<((*nx)*(*ny)); i++){
    old_data = data_array[i];
    data_array[i] = i;  
    printf ("       + [%d] = %d -> %d\n", i, old_data, data_array[i]);
  }

  *error_flag = 0;
  // *error_flag = -2; // Frame can't be read

  return;
}




void get_header ( int *nx, int *ny, int *nbyte, float *qx, float *qy, 
		  int *number_of_frames, int info_array[], int *error_flag){
  /*    Arguments:
	'nx' (*int)              output  number of pixels along x 
	'ny' (*int)              output  number of pixels along y
	'nbyte' (*int)           output  number of bytes in the image... x*y*depth
	'qx' (float)             output  pixel size
	'qy' (float)             output  pixel size
	'number_of_frames (*int) output  number of available frames
	'info' (int [])          output  array of (1024) integers:
                                          - info(0)       = Dectris 
                                          - info(1)       = version number of the library
                                          - info(2..1023) = unused
				      
        'error_flag' (integer)   output  Provides error state condition
                                          0 Success
                                         -1 Library not loaded (provided on the fortran side)
                                         -2 Header cannot be read
  */


  // Fill in dummy values
  *nx       = 3;
  *ny       = 5;
  int depth = 32;

  *nbyte = depth * (*nx) * (*ny);
  printf(" [C] - get_header\n");
  printf("       + nx,ny            = < %d, %d >\n", *nx, *ny);
  printf("       + nbyte            = < %d >\n", *nbyte);

  *qx         = 75.e-6;
  *qy         = 75.e-6;
  printf("       + qx,qy            = < %f, %f >\n", *qx, *qy);
 
  *number_of_frames = 3;
  printf("       + number_of_frames = < %d >\n", *number_of_frames);

  info_array[0] = 100;
  info_array[1] = 200;
  printf("       + array_info[0]    = < %d >\n", info_array[0]);
  printf("       + array_info[1]    = < %d >\n", info_array[1]);
 

  *error_flag = 0;
  printf("       + error_flag       = < %d > \n", *error_flag);


  // *error_flag = -2; // Header can't be read

  return;
}

