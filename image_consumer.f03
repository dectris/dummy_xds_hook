!
! Copyright (C) 2016
! Dectris Ltd., Taefernweg 1, 5405 Baden-Daettwil, Switzerland.
! All rights reserved.
!
! vittorio.boccone@dectris.com
!



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
         ! int strlen(char *string)
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
   integer, parameter, public :: rtld_lazy=1, rtld_now=2, rtld_global=256, rtld_local=0
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
module generic_source
  use iso_c_binding
  implicit none

  character(kind=c_char,len=1024) :: dll_name
  integer(c_int)                  :: status

  public                          :: generic_source_open !, generic_source_header, generic_source_data, generic_source_clone


  common /generic_vars/ dll_name, status

contains

  ! Open the shared-object 
  ! 
  subroutine generic_source_open(handle, detector, template_name, error_flag)
    ! Requirements:
    !  (!) 'ID' (integer 0..255) input  Unique identifier similar to a UNIT, or an FD in C (In the latter case it would then, however,
    !                                   output value that is awarded by the Library)
    !                                   (!) -> Using handle instead (c_ptr)
    !      'DETECTOR'            input  (I would without the  .so, because it is an implementation detail)
    !      'NAME_TEMPLATE'       input  (the Resource in HDF5 masterFile)
    !      'ERROR_FLAG'          output Return values
    !                                         0 Success
    !                                   (!)  -1 ID already exists (not yet implemented)
    !                                        -2 Library can not be loaded
    !                                   (!)  -3 Master file cannot be opened (not yet implemented)
    !                                        -4 function not found in library
    !
    use iso_c_binding
    use iso_c_utilities
    use dlfcn
    implicit none    

    character(kind=c_char,len=1024)    :: sub_name="generic_source_data"
    character(len=:), allocatable      :: detector, template_name
    type(c_ptr)                        :: handle
    integer                            :: error_flag

    write (*, *) "[F] - generic_source_open  <", handle,"> "
    write (*, *) "      detector       = " , detector
    write (*, *) "      template_name  = " , template_name 

 
    dll_name=detector//".so"
    error_flag = 0
    ! Open the DL:
    ! The use of IOR is not really proper...wait till Fortran 2008  
    handle=dlopen(trim(dll_name)//C_NULL_CHAR, IOR(RTLD_NOW, RTLD_GLOBAL))

    ! Check if can use handle
    if(.not.c_associated(handle)) then
       write(*,*) "[X] - error in dlopen: ", c_f_string(dlerror())
       error_flag = -2
       return
    end if

    write (*, *) "[F] - generic_source_open  <", handle,">"
    
    return     
  end subroutine generic_source_open


  ! Get the header
  ! 
  subroutine generic_source_header(handle, nx, ny, nbyte, qx, qy, info_array, error_flag)
    ! Requirements:
    !  (!) 'ID' (integer 0..255)  input   Unique identifier similar to a UNIT, or an FD in C (In the latter case it would then, however,
    !                                     output value that is awarded by the Library)
    !                                     (!) -> Using handle instead (c_ptr)
    !      'NX' (integer)         output  Number of pixels along X 
    !      'NY' (integer)         output  Number of pixels along Y
    !      'NBYTE' (integer)      output  Number of bytes in the image... X*Y*DEPTH
    !      'QX' (4*REAL)          output 
    !      'QY' (4*REAL)          output 
    !      'INFO' (integer array) output  Array of (1024) integers:
    !                                      INFO(1) = Dectris
    !                                      INFO(2) = Version number of the library
    !  (!) 'ERROR_FLAG' (integer) output  Skipped as it's a function.

    ! result(error_flag):
    !        0 Success
    !  (!)  -1 Cannot open id/handle (not yet implemente)
    !       -2 Library can not be loaded
    !  (!)  -3 Cannot read header (not yet implemente)
    !       -4 Cannot find subrouting in the shared-object
    !
    use iso_c_binding
    use iso_c_utilities
    use dlfcn
    implicit none    
    
    ! the dynamic subroutine interface for generic_source_data
    abstract interface
       subroutine get_header(nx, ny, nbyte, qx, qy, info_array, error_flag) bind(C)
         use iso_c_binding
         integer      :: nx, ny, nbyte
         real(kind=4) :: qx, qy
         integer      :: error_flag
         integer, dimension(1024) :: info_array
         
         !real(c_double), value :: x
       end subroutine get_header
    end interface
    procedure(get_header), pointer :: dll_get_header ! dynamically-linked procedure
    
    character(kind=c_char,len=1024)    :: sub_name="generic_source_data"
    character(len=:), allocatable      :: detector, template_name
    type(c_ptr)                        :: handle
    type(c_funptr)                     :: funptr=c_null_funptr
    integer                            :: nx, ny, nbyte
    real(kind=4)                       :: qx, qy
    integer                            :: error_flag
    integer                            :: external_error_flag
    integer, dimension(1024)           :: info_array
    error_flag=0

    write (*, *) "[F] - generic_source_open  <", handle,"> "
    write (*, *) "      detector       = " , detector
    write (*, *) "      template_name  = " , template_name 

    ! Check if can use handle
    if(.not.c_associated(handle)) then
       write(*,*) "[X] - error in dlopen: ", c_f_string(dlerror())
       error_flag = -2
       return
    end if
 
    ! Find the subroutine in the DL:
    funptr=DLSym(handle,"get_header")
    if(.not.c_associated(funptr)) then
       write(*,*) "[X] - error in dlsym: ", c_f_string(dlerror())
       error_flag = -4
       return
    end if

    ! convert the c function pointer to a fortran procedure pointer
    call c_f_procpointer(cptr=funptr, fptr=dll_get_header)
 
    ! finally, invoke the dynamically-linked subroutine:
    call dll_get_header(nx, ny, nbyte, qx, qy, info_array, external_error_flag)

    error_flag = external_error_flag
    return 
  end subroutine generic_source_header


  ! Dynamically map function and execute it 
  ! 
  subroutine generic_source_data(handle, nx, ny, image, error_flag)
    use iso_c_binding
    use iso_c_utilities
    use dlfcn
    implicit none    
    type(c_ptr)                          :: handle
    integer                              :: nx, ny
    type(c_funptr)                       :: funptr=c_null_funptr
    integer                              :: error_flag
    integer (c_int32_t), dimension (:,:) :: image

    write (*, *) "[F] - generic_source_data  <", handle,">", nx, ny, error_flag, image

    ! Find the subroutine in the DL:
    funptr=DLSym(handle,"generic_source_data")
    if(.not.c_associated(funptr)) then
       write(*,*) "[X] - error in dlsym: ", c_f_string(dlerror())
       error_flag = -4
    end if
    ! now convert the c function pointer to a fortran procedure pointer
    ! call c_f_procpointer(cptr=funptr, fptr=dll_external_image_read)
 
  !   ! finally, invoke the dynamically-linked subroutine:
  !   call dll_sub(image)
  !   write (*, *) "[F] - image:",image

  end subroutine generic_source_data

  ! Close the shared-object 
  ! 
  subroutine generic_source_close(handle, error_flag)
    ! Requirements:
    !  (!) 'ID' (integer 0..255) input. Unique identifier similar to a UNIT, or an FD in C (In the latter case it would then, however,
    !                            output value that is awarded by the Library)
    !                            (!) -> Using handle instead (c_ptr)
    !      'ERROR_FLAG' integer  output Return values:
    !                                     0 Success
    !                                    -1 Error closing shared-object

    use iso_c_binding
    use iso_c_utilities
    use dlfcn
    implicit none    

    type(c_ptr) :: handle
    integer     :: error_flag

    write (*, *) "[F] - generic_source_close <", handle,">"

    ! now close the dl:
    status=dlclose(handle)
    if(status/=0) then
       write(*,*) "[X] - error in dlclose: ", c_f_string(dlerror())
       error_flag = -1
    else
       error_flag = 0
    end if

    return 
  end subroutine generic_source_close

end module generic_source


!
! Dummy shared object consumer
!
program image_consumer
  use iso_c_binding
  use generic_source

  implicit none
  integer (c_int32_t), dimension (:,:), allocatable :: image


  integer                         :: number_of_arguments,cptArg
  logical                         :: external_source_flag=.FALSE.
  character(len=20)               :: name
  character(len=:), allocatable   :: detector, template_name
  type(c_ptr)                     :: handle=c_null_ptr
  integer                         :: error_flag
  integer                         :: nx=0, ny=0, nbyte=0
  real(kind=4)                    :: qx=0, qy=0
  integer, dimension(1024)        :: info_array

  number_of_arguments=command_argument_count()

  if(number_of_arguments == 1 ) then
     call get_command_argument(1,name)
          select case(adjustl(name))
     case("--ext")
        external_source_flag=.TRUE.
     case default
        external_source_flag=.FALSE.
     end select
  endif
  write (*, *) "[F] - External_source_flag:", external_source_flag
 
  if (external_source_flag) then
     write (*, *) "[F] - Loading shared-object"
     detector      = 'libDectrisSource'
     template_name = 'path_to_hdf5_master_file'

     call generic_source_open(handle, detector, template_name, error_flag)

     if (0/=error_flag) then
        stop
     else
     
        call generic_source_header(handle, nx, ny, nbyte, qx, qy, info_array, error_flag) ! INFO_ARRAY, error_flag)
        write (*, *) "[F] - generic_source_header"
        write (*, *) "      + handle        = <", handle,">"
        write (*, *) "      + nx,ny         = <", nx, ", ", ny,">"
        write (*, *) "      + nbyte         = <", nbyte,">"
        write (*, *) "      + qx,qy         = <", qx, ", ", qy,">"
        write (*, *) "      + info_array(1) = <", info_array(1) ,">"
        write (*, *) "      + info_array(2) = <", info_array(2) ,">"
        write (*, *) "      + error_flag    = <", error_flag,">"

        ! error_flag = generic_source_data(handle, nx, ny, image, error_flag)
        call generic_source_close(handle, error_flag)
     endif
  else
     write (*, *) "[F] - No shared-object required"
  endif

  stop
  
end program image_consumer
























!
! All in one
! External Image Read (thought c/c++ code)
!
subroutine call_external_image_read (image) 
  use iso_c_binding
  use iso_c_utilities
  use dlfcn
  implicit none
  
  integer (c_int32_t), dimension (*) :: image

  ! local variables:
  character(kind=c_char,len=1024) :: dll_name, sub_name
  type(c_ptr)                     :: handle=c_null_ptr
  type(c_funptr)                  :: funptr=c_null_funptr
  integer(c_int)                  :: status
  
  ! the dynamic subroutine has a simple interface:
  abstract interface
     subroutine external_image_read(image) bind(C)
       use iso_c_binding
       integer (c_int32_t), dimension (1) :: image
       !real(c_double), value :: x
     end subroutine external_image_read
  end interface
  procedure(external_image_read), pointer :: dll_sub ! dynamically-linked procedure
  dll_name="libDectrisSource.so"
  ! Open the DL:
  handle=dlopen(trim(dll_name)//C_NULL_CHAR, IOR(RTLD_NOW, RTLD_GLOBAL))

  ! The use of IOR is not really proper...wait till Fortran 2008  
  if(.not.c_associated(handle)) then
     write(*,*) "error in dlopen: ", c_f_string(dlerror())
     stop
  end if

  ! Find the subroutine in the DL:
  funptr=DLSym(handle,"external_image_read"//C_NULL_CHAR)
  if(.not.c_associated(funptr)) then
     write(*,*) "error in dlsym: ", c_f_string(dlerror())
     stop
  end if
  ! now convert the c function pointer to a fortran procedure pointer
  call c_f_procpointer(cptr=funptr, fptr=dll_sub)
  
  ! finally, invoke the dynamically-linked subroutine:
  call dll_sub(image)
  
   ! now close the dl:
  status=dlclose(handle)
  if(status/=0) then
     write(*,*) "error in dlclose: ", c_f_string(dlerror())
     stop
  end if
  
end subroutine call_external_image_read

!
! External Image Read (thought c/c++ code)
!
