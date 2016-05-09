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
module iso_c_utilities
   use iso_c_binding ! intrinsic module

   character(c_char), dimension(1), save, target, private :: dummy_string="?"
   
contains   
   
   function c_f_string(cptr) result(fptr)
      ! convert a null-terminated c string into a fortran character array pointer
      type(c_ptr), intent(in) :: cptr ! the c address
      character(kind=c_char), dimension(:), pointer :: fptr
      
      interface ! strlen is a standard C function from <string.h>
         function strlen(string) result(len) bind(C,name="strlen")
            use iso_c_binding
            type(c_ptr), value :: string ! a C pointer
         end function
      end interface   
      
      if(c_associated(cptr)) then
         call c_f_pointer(fptr=fptr, cptr=cptr, shape=[strlen(cptr)])
      else
         ! to avoid segfaults, associate fptr with a dummy target:
         fptr=>dummy_string
      end if
            
   end function

end module

!
! Interoperability with C in Fortran 2003
!
! Wrap up module to abstract the interface from 
! http://cims.nyu.edu/~donev/Fortran/DLL/DLL.Forum.txt
!
module dlfcn
   use iso_c_binding
   use iso_c_utilities
   implicit none
   private

   public :: dlopen, dlsym, dlclose, dlerror ! dl api
   
   ! valid modes for mode in dlopen:
   integer(c_int), parameter, public :: rtld_lazy=1, rtld_now=2, rtld_global=256, rtld_local=0
      ! obtained from the output of the previously listed c program 
         
   interface ! all we need is interfaces for the prototypes in <dlfcn.h>
      function dlopen(file,mode) result(handle) bind(C,name="dlopen")
         ! void *dlopen(const char *file, int mode);
         use iso_c_binding
         character(c_char), dimension(*), intent(in) :: file
            ! c strings should be declared as character arrays
         integer(c_int), value :: mode
         type(c_ptr) :: handle
      end function
      function dlsym(handle,name) result(funptr) bind(C,name="dlsym")
         ! void *dlsym(void *handle, const char *name);
         use iso_c_binding
         type(c_ptr), value :: handle
         character(c_char), dimension(*), intent(in) :: name
         type(c_funptr) :: funptr ! a function pointer
      end function
      function dlclose(handle) result(status) bind(C,name="dlclose")
         ! int dlclose(void *handle);
         use iso_c_binding
         type(c_ptr), value :: handle
         integer(c_int) :: status
      end function
      function dlerror() result(error) bind(C,name="dlerror")
         ! char *dlerror(void);
         use iso_c_binding
         type(c_ptr) :: error
      end function         
   end interface
      
 end module dlfcn

!
! Generic handle for share-object like structures
!
! Wrap up module to abstract the interface from 
! http://cims.nyu.edu/~donev/Fortran/DLL/DLL.Forum.txt
!
module generic_data_plugin
  use iso_c_binding
  implicit none

  character(kind=c_char,len=1024) :: dll_filename
  character(kind=c_char,len=1024) :: image_data_filename
  integer(c_int)                  :: status
  type(c_ptr)                     :: handle=c_null_ptr
! for generic_getfrm:
  INTEGER :: nx,ny         
! filled by subroutine parser:
  CHARACTER(len=:), allocatable :: detector

  !public                          :: generic_open !, generic_header, generic_data, generic_clone

  !
  ! Abstract interfaces for C mapped functions
  !
  !
  ! get_header -> dll_get_header 
  abstract interface

     subroutine plugin_open_file(filename, info_array, error_flag) bind(C)
       use iso_c_binding
       integer(c_int)                  :: error_flag
       character(kind=c_char)          :: filename(*)
       integer(c_int), dimension(1024) :: info_array


     end subroutine plugin_open_file

     subroutine plugin_close_file(error_flag) bind(C)
       use iso_c_binding
       integer (c_int)          :: error_flag

     end subroutine plugin_close_file

     subroutine plugin_get_header(nx, ny, nbyte, qx, qy, number_of_frames, info_array, error_flag) bind(C)
       use iso_c_binding
       integer(c_int)                  :: nx, ny, nbyte, number_of_frames
       real(c_float)                   :: qx, qy
       integer(c_int)                  :: error_flag
       integer(c_int), dimension(1024) :: info_array
     end subroutine plugin_get_header

     subroutine plugin_get_data(frame_number, nx, ny, data_array, info_array, error_flag) bind(C)
       use iso_c_binding
       integer(c_int)                   :: nx, ny, frame_number
       integer(c_int)                   :: error_flag
       integer(c_int), dimension(nx:ny) :: data_array
       integer(c_int), dimension(1024)  :: info_array
     end subroutine plugin_get_data
  end interface

  ! dynamically-linked procedures
  procedure(plugin_open_file),  pointer :: dll_plugin_open_file
  procedure(plugin_get_header), pointer :: dll_plugin_get_header 
  procedure(plugin_get_data),   pointer :: dll_plugin_get_data   
  procedure(plugin_close_file), pointer :: dll_plugin_close_file
   



contains

  subroutine generic_getfrm(TEST,XFRM,ACTNAM,NXNY,IFRAME,NX0,NY0,QX0,QY0,IER)
    INTENT(IN)       :: TEST,XFRM,ACTNAM,NXNY   ! test,xfrm are ignored
    INTENT(OUT)      :: IFRAME,NX0,NY0,QX0,QY0,IER
    CHARACTER(len=*) :: ACTNAM
    INTEGER             TEST,XFRM,IER
    INTEGER(4)          NX0,NY0,NXNY,IFRAME(NXNY)
    REAL(4)             QX0,QY0
    INTEGER          :: nbyte,info_array(1024),number_of_frames,len,numfrm
    REAL             :: qx,qy
    CHARACTER(len=:), ALLOCATABLE :: master_file
    
    !PRINT*,'generic_getfrm:',detector
    ier=-3  ! wrong data fromat
    IF (.NOT.ALLOCATED(detector)) RETURN
    IF (detector(1:3)/='lib') RETURN
    len=LEN_TRIM(actnam)             ! test_123456.h5 is a possible actnam
    !  print*,actnam(len-8:len-3)
    READ(actnam(len-8:len-3),*) numfrm  ! this assumes that actnam ends with .h5
    !PRINT*,'actnam=',TRIM(actnam),numfrm,len
    IF (NXNY==0) THEN
       master_file=actnam(:len-9)//'master.h5'
       PRINT*,'master_file=',TRIM(master_file)
       CALL generic_open_file(detector, master_file,info_array, ier)
       IF (ier/=0) THEN
          WRITE(*,*) 'could not open ',detector//'.so',' ier=',ier
          WRITE(*,*) 'check LD_LIBRARY_PATH !'
          ier=-3
          RETURN
       END IF
       CALL generic_get_header(nx,ny,nbyte,qx,qy,number_of_frames,info_array,ier)
       PRINT*,'nx,ny,nbyte,qx,qy,number_of_frames,info_array(1:5)=', &
            nx,ny,nbyte,qx,qy,number_of_frames,info_array(1:5)
       IF (ier/=0) THEN
          WRITE(*,*) 'could not get header from ',TRIM(master_file),' ier=',ier
          ier=-3
          RETURN 
       END IF
       WRITE(*,'(a,4i4,i12)')'INFO(1:5)=vendor/major version/minor version/patch/timestamp=', &
            info_array(1:5)
       IF (info_array(1)/=1) THEN
          WRITE(*,*) 'expected info_array(1)=1 but got',info_array(1) ! 1=Dectris
          ier=-3
          RETURN
       END IF
       IF (info_array(2)/=0) THEN  
          WRITE(*,*) 'expected info_array(2)=0 but got',info_array(2) ! Version 0
          ier=-3
          RETURN
       END IF
       nx0=nx
       ny0=ny
       qx0=qx
       qy0=qy
       IF (info_array(1)==1) THEN ! Dectris reports pixel size in m, not mm
          qx0=qx0*1000
          qy0=qy0*1000
       END IF
       ! at this point, ier has a meaningful value
    ELSE IF (nxny<nx*ny) THEN
       WRITE(*,*) 'not enough space in iframe array'
       ier=-3
    ELSE
       CALL generic_get_data(numfrm, nx, ny, iframe, info_array, ier)
       IF (ier<0) THEN
          WRITE(*,*)'error from generic_get_data, numfrm, ier=',numfrm,ier
          STOP
          ier=-1
       END IF
    END IF
    ! at this point, ier should have a meaningul value
    PRINT*,'return from generic_getfrm, ier=',ier
  END subroutine generic_getfrm


  ! 
  ! Open the shared-object 
  subroutine generic_open_file(detector, template_name, info_array, error_flag)
    ! Requirements:
    !  'DETECTOR'                     input  (I would without the  .so, because it is an implementation detail)
    !  'TEMPLATE_NAME'                input  (the resource in image data masterfile)
    !  'INFO' (integer array)         input  Array of (1024) integers:
    !                                          INFO(1)    = Consumer ID (1:XDS)
    !                                          INFO(2)    = Version Number of the Consumer software
    !                                          INFO(3:8)  = Unused
    !                                          INFO(9:40) = 1024bit signature of the consumer software
    !                                          INFO(>41)  = Unused
    !  'INFO' (integer array)         output Array of (1024) integers:
    !                                          INFO(1)    = Vendor ID (1:Dectris)
    !                                          INFO(2)    = Major Version number of the library
    !                                          INFO(3)    = Minor Version number of the library
    !                                          INFO(4)    = Parch Version number of the library
    !                                          INFO(5)    = Linux timestamp of library creation
    !                                          INFO(6:8)  = Unused
    !                                          INFO(9:40) = 1024bit signature of the library
    !                                          INFO(>41)  = Unused
    !  'ERROR_FLAG'                   output Return values
    !                                         0 Success
    !                                        -1 Handle already exists
    !                                        -2 Cannot open Library
    !                                        -3 Function not found in library
    !                                        -4 Master file cannot be opened (coming from C function)
    !                                        -10 Consumer identity not supported (coming from C function)
    !                                        -11 Consumer identity could not be verified (coming from C function)
    !                                        -12 Consumer software version not supported (coming from C function)

    use iso_c_binding
    use iso_c_utilities
    use dlfcn
    implicit none    

    character(len=:), allocatable      :: detector, template_name
    integer(c_int)                     :: error_flag
    integer(c_int), dimension(1024)    :: info_array
    type(c_funptr)                     :: fun_plugin_open_file_ptr   = c_null_funptr
    type(c_funptr)                     :: fun_plugin_close_file_ptr  = c_null_funptr
    type(c_funptr)                     :: fun_plugin_get_header_ptr  = c_null_funptr
    type(c_funptr)                     :: fun_plugin_get_data_ptr    = c_null_funptr
    integer(c_int)                     :: external_error_flag


    error_flag=0

    write (*,*) "[F] - generic_open_file"
    write (*,*) "      + detector          = <", detector,      ">"
    write (*,*) "      + template_name     = <", template_name, ">"
    write (*,*)  "      + handle (original) = <", handle,        ">"

    if ( c_associated(handle) ) then
       write(*,*) "[X] - error. 'handle' not null"
       error_flag = -1
       return
    endif

    dll_filename=detector//".so"
    ! error_flag = 0 
    write (*,*)  "      + dll_filename    = <", trim(dll_filename)//C_NULL_CHAR, ">"
 
    image_data_filename=trim(template_name)//C_NULL_CHAR
    ! error_flag = 0 
    write (*,*)  "      + image_data_filename   = <", trim(image_data_filename)//C_NULL_CHAR, ">"

    !
    ! Open the DL:
    ! The use of IOR is not really proper...wait till Fortran 2008  
    handle=dlopen(trim(dll_filename)//C_NULL_CHAR, IOR(RTLD_NOW, RTLD_GLOBAL))

    !
    ! Check if can use handle
    if(.not.c_associated(handle)) then
       write(*,*) "[X] - error in dlopen: ", c_f_string(dlerror())
       error_flag = -2
       return
    end if
    
    write (*,*)  "      + handle (new)      = <", handle,        ">"

    !
    ! Find the subroutines in the DL:
    fun_plugin_get_data_ptr   = DLSym(handle,"plugin_get_data")
    if(.not.c_associated(fun_plugin_get_data_ptr))  then
       write(*,*) "[X] - error in dlsym: ", c_f_string(dlerror())
       error_flag = -3
    else
       call c_f_procpointer(cptr=fun_plugin_get_data_ptr,   fptr=dll_plugin_get_data)
    endif
    !
    fun_plugin_get_header_ptr = DLSym(handle,"plugin_get_header")
    if(.not.c_associated(fun_plugin_get_header_ptr))  then
       write(*,*) "[X] - error in dlsym: ", c_f_string(dlerror())
       error_flag = -3
    else
       call c_f_procpointer(cptr=fun_plugin_get_header_ptr, fptr=dll_plugin_get_header)
    endif
    !
    fun_plugin_open_file_ptr   = DLSym(handle,"plugin_open_file")
    if(.not.c_associated(fun_plugin_open_file_ptr))  then
       write(*,*) "[X] - error in dlsym: ", c_f_string(dlerror())
       error_flag = -3
    else
       call c_f_procpointer(cptr=fun_plugin_open_file_ptr,   fptr=dll_plugin_open_file)
    endif
    !
    fun_plugin_close_file_ptr = DLSym(handle,"plugin_close_file")
    if(.not.c_associated(fun_plugin_close_file_ptr)) then
       write(*,*) "[X] - error in dlsym: ", c_f_string(dlerror())
       error_flag = -3
    else
       call c_f_procpointer(cptr=fun_plugin_close_file_ptr, fptr=dll_plugin_close_file)
    endif


    if (0/=error_flag) then
       return
    endif
       
    call dll_plugin_open_file(image_data_filename, info_array, external_error_flag)
    error_flag = external_error_flag

    return     
  end subroutine generic_open_file

  !
  ! Get the header
  subroutine generic_get_header(nx, ny, nbyte, qx, qy, number_of_frames, info_array, error_flag)
    ! Requirements:
    !  'NX' (integer)                  output  Number of pixels along X 
    !  'NY' (integer)                  output  Number of pixels along Y
    !  'NBYTE' (integer)               output  Number of bytes in the image... X*Y*DEPTH
    !  'QX' (4*REAL)                   output  Pixel size
    !  'QY' (4*REAL)                   output  Pixel size
    !  'NUMBER_OF_FRAMES' (integer)    output  Number of frames for the full datase. So far unused
    !  'INFO' (integer array)           input  Array of (1024) integers:
    !                                          INFO(>1)     = Unused
    !  'INFO' (integer array)          output  Array of (1024) integers:
    !                                           INFO(1)       = Vendor ID (1:Dectris)
    !                                           INFO(2)       = Major Version number of the library
    !                                           INFO(3)       = Minor Version number of the library
    !                                           INFO(4)       = Patch Version number of the library
    !                                           INFO(5)       = Linux timestamp of library creation
    !                                           INFO(6:64)    = Reserved
    !                                           INFO(65:1024) = Dataset parameters
    !  'ERROR_FLAG'                    output  Return values
    !                                            0      Success
    !                                           -1      Cannot open library
    !                                           -2      Cannot read header (will come from C function)
    !                                           -4      Cannot read dataset informations (will come from C function)
    !                                           -10     Error in the determination of the Dataset parameters (will come from C function)
    !
    use iso_c_binding
    use iso_c_utilities
    use dlfcn
    implicit none    
    
    integer(c_int)                   :: nx, ny, nbyte, number_of_frames
    real(c_float)                    :: qx, qy
    integer(c_int)                   :: error_flag
    integer(c_int)                   :: external_error_flag
    integer(c_int), dimension(1024)  :: info_array
    error_flag=0

    write (*,*) "[F] - generic_get_header"
    write (*,*)  "      + handle            = <", handle,        ">"

    ! Check if can use handle
    if(.not.c_associated(handle)) then
       write(*,*) "[X] - error in dlopen: ", c_f_string(dlerror())
       error_flag = -1
       return
    end if
 
    ! finally, invoke the dynamically-linked subroutine:
    call dll_plugin_get_header(nx, ny, nbyte, qx, qy, number_of_frames, info_array, external_error_flag)

    error_flag = external_error_flag
    return 
  end subroutine generic_get_header


  ! 
  ! Dynamically map function and execute it 
  subroutine generic_get_data(frame_number, nx, ny, data_array, info_array, error_flag)
    ! Requirements:
    !  'FRAME_NUMBER' (integer)        input  Number of frames for the full datase. So far unused
    !  'NX' (integer)                  input  Number of pixels along X 
    !  'NY' (integer)                  input  Number of pixels along Y
    !  'DATA_ARRAY' (integer array)   output  1D array containing pixel data with lenght = NX*NY
    !  'INFO' (integer array)         output Array of (1024) integers:
    !                                          INFO(1)     = Vendor ID (1:Dectris)
    !                                          INFO(2)     = Major Version number of the library
    !                                          INFO(3)     = Minor Version number of the library
    !                                          INFO(4)     = Parch Version number of the library
    !                                          INFO(5)     = Linux timestamp of library creation
    !                                          INFO(6:8)   = Unused
    !                                          INFO(9:40)  = 1024bit verification key
    !                                          INFO(41:44) = Image MD5 Checksum 
    !                                          INFO()  = Unused
    !  'ERROR_FLAG' (integer)         output  Provides error state condition
    !                                           0 Success
    !                                          -1 Cannot open library 
    !                                          -2 Cannot open frame (will come from C function)
    !                                          -3 Datatype not supported (will come from C function)
    !                                          -4 Cannot read dataset informations (will come from C function)
    !                                         -10 MD5 Checksum Error 
    !                                         -11 Verification key error
    !  
    use iso_c_binding
    use iso_c_utilities
    use dlfcn
    implicit none    

    integer(c_int)                    :: nx, ny, frame_number
    integer(c_int)                    :: error_flag
    integer(c_int), dimension(1024)   :: info_array
    integer(c_int), dimension (nx*ny) :: data_array

    error_flag = 0
    ! Check if can use handle
    ! if(.not.c_associated(handle)) then
    !    write(*,*) "[X] - error in dlopen: ", c_f_string(dlerror())
    !    error_flag = -1
    !    return
    ! end if

    ! write (*,*) "[F] - generic_get_data"
    ! write (*,*) "      + handle       = <", handle,       ">"
    ! write (*,*) "      + frame_number = <", frame_number, ">"
    ! write (*,*) "      + nx, ny       = <", nx, ",", ny,  ">"
 
    ! invoke the dynamically-linked subroutine:
    call dll_plugin_get_data(frame_number, nx, ny, data_array, info_array, error_flag)
    
  end subroutine generic_get_data

  ! Close the shared-object 
  ! 
  subroutine generic_close_file(error_flag)
    ! Requirements:
    !      'ERROR_FLAG' (integer)     output  Return values:
    !                                           0 Success
    !                                          -1 Error closing Masterfile
    !                                          -2 Error closing Shared-object

    use iso_c_binding
    use iso_c_utilities
    use dlfcn
    implicit none    

    integer(c_int) :: error_flag
    integer(c_int) :: external_error_flag

    ! Check if can use handle
    if(.not.c_associated(handle)) then
       write(*,*) "[X] - error in dlopen: ", c_f_string(dlerror())
       error_flag = -1
       return
    end if

    write (*,*) "[F] - generic_close_file"
    write (*,*) "      + handle       = <", handle,">"
    
    call dll_plugin_close_file(external_error_flag)
    error_flag = external_error_flag

    ! now close the dl:
    status=dlclose(handle)
    if(status/=0) then
       write(*,*) "[X] - error in dlclose: ", c_f_string(dlerror())
       error_flag = -2
    else
       error_flag = 0
    end if

    return 
  end subroutine generic_close_file

end module generic_data_plugin

!
! Dummy shared object consumer
!
program image_consumer
  use iso_c_binding
  use generic_data_plugin
  
  implicit none



  integer                                       :: number_of_arguments, cptArg
  character(len=200)                            ::name !Arg name
  logical                                       :: external_source_flag=.FALSE.
  character(len=:), allocatable                 :: template_name
  integer                                       :: i
  ! character(len=20)                             :: name
  ! character(len=:), allocatable                 :: detector,
  integer(c_int)                                :: error_flag, number_of_frames, nbyte=0, frame_number
  ! integer(c_int)                                :: nx=0, ny=0, frame_number
  real(c_float)                                 :: qx=0, qy=0
  integer(c_int), dimension(1024)               :: info_array
  integer(c_int), dimension (:,:), allocatable  :: data_array

  number_of_arguments=command_argument_count()
  if(number_of_arguments == 1) then
     call get_command_argument(1,name)
  else
     write (*,*) "[F] - Pass the filename of the masterfile"
     stop 
  endif
  
  write (*,*) "[F] - Loading shared-object"
  detector      = 'libdectrish5toxds'
  template_name = adjustl(name)

  info_array(1) = 1 ! XDS 
  info_array(2) = 123456789 ! XDS dummy version

  call generic_open_file(detector, template_name, info_array, error_flag)

  if (0/=error_flag) then
     stop
  else

     call generic_get_header(nx, ny, nbyte, qx, qy, number_of_frames, info_array, error_flag) ! INFO_ARRAY, error_flag)
     write (*,*) "[F] - generic_header"
     write (*,*) "      + nx,ny            = <", nx, ", ", ny,">"
     write (*,*) "      + nbyte            = <", nbyte,">"
     write (*,*) "      + qx,qy            = <", qx, ", ", qy,">"
     write (*,*) "      + number_of_frames = <",number_of_frames ,">"
     write (*,*) "      + info_array(1)    = <", info_array(1) ,">"
     write (*,*) "      + info_array(2)    = <", info_array(2) ,">"
     write (*,*) "      + error_flag       = <", error_flag,">"
     
     if (0/=error_flag) then
        stop
     else

        ! One must place the total number of frames somewhere in the info array
        allocate (data_array(nx,ny))
        frame_number=1
!        do frame_number = 1,number_of_frames
!          write (*,*) "[F] - [FRAME n.",frame_number,"]"

!          ! data_array(1,1)  =   1+frame_number
!          ! data_array(3,2)  =   5+frame_number
!          ! data_array(5,3) =  10+frame_number

        call generic_get_data(frame_number, nx, ny, data_array, info_array, error_flag)

        write (*,*) "[F] - generic_data"
        write (*,*) "      + frame_number       = <", frame_number, ">"
        write (*,*) "      + nx,ny              = <", nx, ", ", ny, ">"
        do i=1, 1065, 1
           write (*,*) data_array(1,i), data_array(2,i), data_array(3,i), data_array(4,i)
        end do
        write (*,*) "      + error_flag         = <", error_flag,   ">"
!       end do
     endif

     call generic_close_file(error_flag)
  endif

  stop
  
end program image_consumer
