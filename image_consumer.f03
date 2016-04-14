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
  type(c_ptr)                     :: handle=c_null_ptr

  !public                          :: generic_source_open !, generic_source_header, generic_source_data, generic_source_clone

  !
  ! Abstract interfaces for C mapped functions
  !
  !
  ! get_header -> dll_get_header 
  abstract interface
     subroutine get_header(nx, ny, nbyte, qx, qy, info_array, error_flag) bind(C)
       use iso_c_binding
       integer                  :: nx, ny, nbyte
       real(kind=4)             :: qx, qy
       integer                  :: error_flag
       integer, dimension(1024) :: info_array
     end subroutine get_header
  end interface
  procedure(get_header), pointer :: dll_get_header ! dynamically-linked procedure
  !
  ! get_data -> dll_get_data
  abstract interface
     subroutine get_data(frame_number, nx, ny, data_array, error_flag) bind(C)
       use iso_c_binding
       integer                                    :: nx, ny, frame_number
       integer                                    :: error_flag
       integer(kind=4), dimension(:), allocatable :: data_array
     end subroutine get_data
  end interface
  procedure(get_data), pointer :: dll_get_data ! dynamically-linked procedure
    


  common /generic_vars/ dll_name, status

contains

  ! Open the shared-object 
  ! 
  subroutine generic_source_open(detector, template_name, error_flag)
    ! Requirements:
    !  (!) 'ID' (integer 0..255) input  Unique identifier similar to a UNIT, or an FD in C (In the latter case it would then, however,
    !                                   output value that is awarded by the Library)
    !                                   (!) Not implemented
    !      'DETECTOR'            input  (I would without the  .so, because it is an implementation detail)
    !      'NAME_TEMPLATE'       input  (the Resource in HDF5 masterFile)
    !      'ERROR_FLAG'          output Return values
    !                                         0 Success
    !                                        -1 Handle already exists
    !                                        -2 Library not loaded
    !                                   (!)  -3 Master file cannot be opened (not yet implemented)
    !                                        -4 Function not found  n library
    !
    use iso_c_binding
    use iso_c_utilities
    use dlfcn
    implicit none    

    character(len=:), allocatable      :: detector, template_name
    integer                            :: error_flag
    type(c_funptr)                     :: fun_get_header_ptr=c_null_funptr
    type(c_funptr)                     :: fun_get_data_ptr=c_null_funptr
    !type(c_ptr)                        :: handleA=c_null_ptr
    !type(c_ptr)                        :: handleB=c_null_ptr

    write (*, *) "[F] - generic_source_open"
    write (*, *) "      + detector          = <", detector,      ">"
    write (*, *) "      + template_name     = <", template_name, ">"
    write (*,*)  "      + handle (original) = <", handle,        ">"

    if ( c_associated(handle) ) then
       write(*,*) "[X] - error. 'handle' not null"
       error_flag = -1
       return
    endif



    dll_name=detector//".so"
    error_flag = 0 
    write (*,*)  "      + dll name         = <", trim(dll_name)//C_NULL_CHAR, ">"

    ! Open the DL:
    ! The use of IOR is not really proper...wait till Fortran 2008  
    handle=dlopen(trim(dll_name)//C_NULL_CHAR, IOR(RTLD_NOW, RTLD_GLOBAL))


    !handleA=dlopen(trim(dll_name)//C_NULL_CHAR, IOR(RTLD_NOW, RTLD_GLOBAL))
    !write (*,*)  "      + handleA            = <", handleA,        ">"
    !handleB=dlopen(trim(dll_name)//C_NULL_CHAR, IOR(RTLD_NOW, RTLD_GLOBAL))
    !write (*,*)  "      + handleB            = <", handleB,        ">"

    ! Check if can use handle
    if(.not.c_associated(handle)) then
       write(*,*) "[X] - error in dlopen: ", c_f_string(dlerror())
       error_flag = -2
       return
    end if
    
    write (*,*)  "      + handle (new)      = <", handle,        ">"

    !
    ! Find the subroutines in the DL:
    fun_get_data_ptr   = DLSym(handle,"get_data")
    if(.not.c_associated(fun_get_data_ptr))  then
       write(*,*) "[X] - error in dlsym: ", c_f_string(dlerror())
       error_flag = -4
    else
       call c_f_procpointer(cptr=fun_get_data_ptr,   fptr=dll_get_data)
    endif
    !
    fun_get_header_ptr = DLSym(handle,"get_header")
    if(.not.c_associated(fun_get_header_ptr))  then
       write(*,*) "[X] - error in dlsym: ", c_f_string(dlerror())
       error_flag = -4
    else
       call c_f_procpointer(cptr=fun_get_header_ptr, fptr=dll_get_header)
    endif
 
    
    return     
  end subroutine generic_source_open


  ! Get the header
  ! 
  subroutine generic_source_header(nx, ny, nbyte, qx, qy, info_array, error_flag)
    ! Requirements:
    !  (!) 'ID' (integer 0..255)  input   Unique identifier similar to a UNIT, or an FD in C (In the latter case it would then, however,
    !                                     output value that is awarded by the Library)
    !                                     (!) -> Not implemented
    !      'NX' (integer)         output  Number of pixels along X 
    !      'NY' (integer)         output  Number of pixels along Y
    !      'NBYTE' (integer)      output  Number of bytes in the image... X*Y*DEPTH
    !      'QX' (4*REAL)          output  pixel size
    !      'QY' (4*REAL)          output  pixel size
    !      'INFO' (integer array) output  Array of (1024) integers:
    !                                      INFO(1) = Dectris
    !                                      INFO(2) = Version number of the library
    !  (!) 'ERROR_FLAG' (integer) output  Provides error state condition
    !                                      0 Success
    !                                     -1 Library not loaded
    !                                     -2 Cannot read header (will come from C function)
    !
    use iso_c_binding
    use iso_c_utilities
    use dlfcn
    implicit none    
    
    integer                            :: nx, ny, nbyte
    real(kind=4)                       :: qx, qy
    integer                            :: error_flag
    integer                            :: external_error_flag
    integer, dimension(1024)           :: info_array
    error_flag=0

    write (*, *) "[F] - generic_source_header"
    write (*,*)  "      + handle            = <", handle,        ">"

    ! Check if can use handle
    if(.not.c_associated(handle)) then
       write(*,*) "[X] - error in dlopen: ", c_f_string(dlerror())
       error_flag = -1
       return
    end if
 
    ! finally, invoke the dynamically-linked subroutine:
    call dll_get_header(nx, ny, nbyte, qx, qy, info_array, external_error_flag)

    error_flag = external_error_flag
    return 
  end subroutine generic_source_header


  ! Dynamically map function and execute it 
  ! 
  subroutine generic_source_data(frame_number, nx, ny, data_array, error_flag)
    use iso_c_binding
    use iso_c_utilities
    use dlfcn
    implicit none    

    integer                                     :: nx, ny, frame_number
    integer                                     :: error_flag
    integer(kind=4), dimension (:), allocatable :: data_array

    write (*, *) "[F] - generic_source_data"
    write (*, *) "      + handle       = <", handle,       ">"
    write (*, *) "      + frame_number = <", frame_number, ">"
    write (*, *) "      + nx, ny       = <", ny, ",", ny,  ">"

 
    !   ! finally, invoke the dynamically-linked subroutine:
    call dll_get_data(frame_number, nx, ny, data_array, error_flag)
    write (*, *) "[F] - data array:"
    
  end subroutine generic_source_data

  ! Close the shared-object 
  ! 
  subroutine generic_source_close(error_flag)
    ! Requirements:
    !  (!) 'ID' (integer 0..255) input. Unique identifier similar to a UNIT, or an FD in C (In the latter case it would then, however,
    !                            output value that is awarded by the Library)
    !                            (!) -> Not implemented
    !      'ERROR_FLAG' integer  output Return values:
    !                                     0 Success
    !                                    -1 Error closing shared-object

    use iso_c_binding
    use iso_c_utilities
    use dlfcn
    implicit none    

    integer     :: error_flag

    write (*, *) "[F] - generic_source_close:handle <", handle,">"

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
  !use iso_c_binding
  use generic_source

  implicit none


  integer                                     :: number_of_arguments, cptArg
  logical                                     :: external_source_flag=.FALSE.
  character(len=20)                           :: name
  character(len=:), allocatable               :: detector, template_name
  integer                                     :: error_flag
  integer                                     :: nx=0, ny=0, nbyte=0, frame_number
  real(kind=4)                                :: qx=0, qy=0
  integer, dimension(1024)                    :: info_array
  integer(kind=4), dimension (:), allocatable :: data_array

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

     call generic_source_open(detector, template_name, error_flag)

     if (0/=error_flag) then
        stop
     else
     
        call generic_source_header(nx, ny, nbyte, qx, qy, info_array, error_flag) ! INFO_ARRAY, error_flag)
        write (*, *) "[F] - generic_source_header"
        write (*, *) "      + nx,ny         = <", nx, ", ", ny,">"
        write (*, *) "      + nbyte         = <", nbyte,">"
        write (*, *) "      + qx,qy         = <", qx, ", ", qy,">"
        write (*, *) "      + info_array(1) = <", info_array(1) ,">"
        write (*, *) "      + info_array(2) = <", info_array(2) ,">"
        write (*, *) "      + error_flag    = <", error_flag,">"


        if (0/=error_flag) then
           stop
        else
           
           ! One must place the total number of frames somewhere in the info array
           frame_number = 1
           allocate (data_array(nx*ny))

           call generic_source_data(frame_number, nx, ny, data_array, error_flag)
           write (*, *) "[F] - generic_source_data"
           write (*, *) "      + nx,ny         = <", nx, ", ", ny, ">"
           write (*, *) "      + data_array    = <", data_array,   ">"
           write (*, *) "      + error_flag    = <", error_flag,   ">"
        endif
        
        call generic_source_close(error_flag)
     endif
  else
     write (*, *) "[F] - No shared-object required"
  endif

  stop
  
end program image_consumer























