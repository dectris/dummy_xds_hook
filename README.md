# dummy_xds_hook

Compile dummy consumer and shared library
> vitb@not-sure-centos7vm:~/Repositories/dummy_xds_hook:(proof_of_concept $)$ make
  /opt/intel/bin/ifort -c -free -fPIC -heap-arrays -O3 -g -traceback -Tf  image_consumer.f03 -o image_consumer.o
  /opt/intel/bin/ifort -g  -o image_consumer image_consumer.o  -ldl
  gcc -g -c -fPIC -Wall -std=gnu99  dectris_source.c -o dectris_source.o
  gcc -shared -fPIC  -o libDectrisSource.so dectris_source.o -lc



Add local dir to LD_LIBRARY_PATH
> export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:.


Run dummy consumer:
> vitb@not-sure-centos7vm:~/Repositories/dummy_xds_hook:[proof_of_concept $%]$ ./image_consumer 
   [F] - Loading shared-object
   [F] - generic_open
         + detector          = <libDectrisSource>
         + template_name     = <path_to_hdf5_master_file/master_file.hdf5>
         + handle (original) = <                     0 >
         + dll_filename    = <libDectrisSource.so>
         + hdf5_filename   = <path_to_hdf5_master_file/master_file.hdf5>
         + handle (new)      = <              39883568 >
   [C] - hdf5_open 'path_to_hdf5_master_file/master_file.hdf5'
   [F] - generic_header
         + handle            = <              39883568 >
   [C] - get_header
         + nx,ny            = < 3, 5 >
         + nbyte            = < 480 >
         + qx,qy            = < 0.000075, 0.000075 >
         + number_of_frames = < 3 >
         + array_info[0]    = < 100 >
         + array_info[1]    = < 200 >
         + error_flag       = < 0 > 
   [F] - generic_header
         + nx,ny            = <           3 ,            5 >
         + nbyte            = <         480 >
         + qx,qy            = <  7.5000004E-05 ,   7.5000004E-05 >
         + number_of_frames = <           3 >
         + info_array(1)    = <         100 >
         + info_array(2)    = <         200 >
         + error_flag       = <           0 >
   [F] - [FRAME n.           1 ]
   [F] - generic_data
         + handle       = <              39883568 >
         + frame_number = <           1 >
         + nx, ny       = <           3 ,           5 >
   [C] - get_data
         + nx,ny         = < 3, 5 >
         + [0] = 2 -> 0
         + [1] = 0 -> 1
         + [2] = 0 -> 2
         + [3] = 0 -> 3
         + [4] = 0 -> 4
         + [5] = 0 -> 5
         + [6] = 0 -> 6
         + [7] = 6 -> 7
         + [8] = 0 -> 8
         + [9] = 0 -> 9
         + [10] = 0 -> 10
         + [11] = 0 -> 11
         + [12] = 0 -> 12
         + [13] = 0 -> 13
         + [14] = 11 -> 14
   [F] - data array:
   [F] - generic_data
         + frame_number  = <           1 >
         + nx,ny         = <           3 ,            5 >
         + data_array    = <           0           1           2           3
             4           5           6           7           8           9
            10          11          12          13          14 >
         + error_flag    = <           0 >
 