!
! This is free and unencumbered software released into the public domain.
!
! Anyone is free to copy, modify, publish, use, compile, sell, or distribute this software, 
! either in source code form or as a compiled binary, for any purpose, commercial or non-commercial,
! and by any means.
!
! In jurisdictions that recognize copyright laws, the author or authors of this software dedicate 
! any and all copyright interest in the software to the public domain. We make this dedication for
! the benefit of the public at large and to the detriment of our heirs and successors. We intend
! this dedication to be an overt act of relinquishment in perpetuity of all present and future 
! rights to this software under copyright law.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
! NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
! IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
! ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR 
! THE USE OR OTHER DEALINGS IN THE SOFTWARE.
!
! For more information, please refer to For more information, please refer to <http://unlicense.org/>
!
!
! vittorio.boccone@dectris.com
! Dectris Ltd., Taefernweg 1, 5405 Baden-Daettwil, Switzerland.
!
! (proof_of_concept)
!
! Interoperability with C in Fortran 2003
!
! Wrap up module to abstract the interface from 
! http://cims.nyu.edu/~donev/Fortran/DLL/DLL.Forum.txt
!

!
! Dummy shared object consumer
!
program image_consumer

  use iso_c_binding
  use generic_data_plugin
  
  implicit none



  integer                                       :: number_of_arguments, cptArg
  character(len=200)                            :: name !Argument name
  logical                                       :: external_source_flag=.FALSE.
  character(len=:), allocatable                 :: template_name
  integer                                       :: i
  integer(c_int)                                :: error_flag, number_of_frames, nbyte=0, frame_number
  real(c_float)                                 :: qx=0, qy=0
  integer(c_int), dimension(1024)               :: metadata_array
  integer(c_int), dimension(1024)               :: info_array
  integer(c_int), dimension(1024)               :: data_validation_array
  integer(c_int), dimension (:,:), allocatable  :: data_array
  INTEGER                         :: nx,ny      ! global variables that do not change    
  CHARACTER(len=:), allocatable   :: library    ! global variable that does not change 

  OPEN ( 6, STATUS='UNKNOWN', RECL=256 )

  number_of_arguments=command_argument_count()
  if(number_of_arguments == 1) then
     call get_command_argument(1,name)
  else
     write (6,*) "[image_consumer] - ERROR - Please specify dataset masterfile"
     call exit(-100) 
  endif
  
  write (6,*) "[image_consumer] - INFO - Loading shared-object"
  library      = 'libdectrish5toxds.so'
  template_name = trim(adjustl(name))

  info_array(1) = 1 ! XDS 
  info_array(2) = 123456789 ! XDS dummy version
  write (6,*) "[image_consumer] - INFO - Mocking XDS info_array input values"
  write (6,*) "      + info_array(1) - Consumer ID  = <", info_array(1),">"
  write (6,*) "      + info_array(2) - Version      = <", info_array(2),">"

  write (6,*) "[image_consumer] - INFO - 'call generic_open()'"
  call generic_open(library, template_name, info_array, error_flag)
  if (0/=error_flag) then
     write (6,*) "      + error_flag       = <", error_flag,">"
     call exit(error_flag)
  else
     
     write (6,*) "[image_consumer] - INFO - 'call generic_get_header()'"
     call generic_get_header( metadata_array, error_flag) ! INFO_ARRAY, error_flag)

     nx               = metadata_array(1)
     ny               = metadata_array(2)
     nbyte            = metadata_array(3)
     qx               = 1E-9 * metadata_array(4)
     qy               = 1E-9 * metadata_array(5)
     number_of_frames = metadata_array(6)

     if (0/=error_flag) then
        write (6,*) "      + error_flag       = <", error_flag,">"
        call exit(error_flag)
     else     
        write (6,*) "      + nx,ny            = <", nx, ", ", ny,     ">"
        write (6,*) "      + nbyte            = <", nbyte,            ">"
        write (6,*) "      + qx,qy            = <", qx, ", ", qy,     ">"
        write (6,*) "      + number_of_frames = <", number_of_frames ,">"
        write (6,*) "      + info_array(1)    = <", info_array(1)    ,">"
        write (6,*) "      + info_array(2)    = <", info_array(2)    ,">"
     
        ! One must place the total number of frames somewhere in the info array
        allocate (data_array(nx,ny))
        frame_number=1
        
        write (6,*) "[image_consumer] - INFO - 'call generic_get_data()'"
        call generic_get_data(frame_number, nx, ny, data_array, data_validation_array, error_flag)
        if (0/=error_flag) then
           write (6,*) "      + error_flag       = <", error_flag,">"
           call exit(error_flag)

        else
           write (6,*) "      + frame_number       = <", frame_number, ">"
           write (6,*) "      + nx,ny              = <", nx, ", ", ny, ">"
           do i=1, 50, 1
              write (6,*) data_array(1,i), data_array(2,i), data_array(3,i), data_array(4,i)
           end do
        endif
     endif

     write (6,*) "[image_consumer] - INFO - 'call generic_close()'"
     call generic_close(error_flag)
  endif
  
  write (6,*) "      + error_flag       = <", error_flag,">"
  call exit(error_flag)


end program image_consumer
