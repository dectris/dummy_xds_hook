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

  ! Define a type for an array of pointers
  ! type pp
  !   type(c_ptr), dimension(:), pointer :: p
  ! end type pp


  ! the dynamic subroutine interface for generic_source_data
  ! abstract interface
  !    subroutine generic_source_data(image) bind(C)
  !      use iso_c_binding
  !      integer (c_int32_t), dimension (1) :: image
  !      !real(c_double), value :: x
  !    end subroutine generic_source_data
  ! end interface
  ! procedure(generic_source_data), pointer :: dll_external_image_read ! dynamically-linked procedure


  public                          :: generic_source_open !, generic_source_header, generic_source_data, generic_source_clone


  common /generic_vars/ dll_name, status

contains

  ! Open the shared-object 
  ! 
  function generic_source_open(handle, detector, template_name) result(return_value)
    ! Requirements:
    ! 'ID' (integer 0..255) input. Unique identifier similar to a UNIT, or an FD in C (In the latter case it would then, however,
    !                       output value that is awarded by the Library)
    ! 'DETECTOR'            input, (I would without the  .so, because it is an implementation detail)
    ! 'NAME_TEMPLATE'       input, (the Resource in HDF5 masterFile)
    !  RETURN:
    !   0 Success
    !  -1 ID already exists, 
    !  -2 Library can not be loaded
    !  -3 Master file cannot be opened
    !
    use iso_c_binding
    use iso_c_utilities
    use dlfcn
    implicit none    


    character(kind=c_char,len=1024)    :: sub_name="generic_source_data"
    character(len=:), allocatable      :: detector, template_name
    type(c_ptr)                        :: handle
    type(c_funptr)                     :: funptr=c_null_funptr
    integer                            :: return_value
    write (*, *) "[I] - generic_source_open  <", handle,">", detector, template_name 

    !do i = 1, size(array)
    !   allocate(array(i)%p(i))
    !end do

    dll_name="libDectrisSource.so"
    ! Open the DL:
    handle=dlopen(trim(dll_name)//C_NULL_CHAR, IOR(RTLD_NOW, RTLD_GLOBAL))
    ! The use of IOR is not really proper...wait till Fortran 2008  
    if(.not.c_associated(handle)) then
       write(*,*) "error in dlopen: ", c_f_string(dlerror())
       return_value = -2
    end if

    ! Find the subroutine in the DL:
    funptr=DLSym(handle,trim(sub_name)//C_NULL_CHAR)
    if(.not.c_associated(funptr)) then
       write(*,*) "error in dlsym: ", c_f_string(dlerror())
       stop
    end if
    ! now convert the c function pointer to a fortran procedure pointer
    ! call c_f_procpointer(cptr=funptr, fptr=dll_external_image_read)
    

    write (*, *) "[I] - generic_source_open  <", handle,">"
    return
  end function generic_source_open


  ! Dynamically map function and execute it 
  ! 
  subroutine generic_source_data(handle, NX, NY, image, error_flag)
    use iso_c_binding
    use iso_c_utilities
    use dlfcn
    implicit none    
    type(c_ptr)                          :: handle
    integer                              :: NX, NY
    logical                              :: error_flag
    integer (c_int32_t), dimension (:,:) :: image

    write (*, *) "[I] - generic_source_data  <", handle,">", NX, NY, error_flag, image

  !   ! finally, invoke the dynamically-linked subroutine:
  !   call dll_sub(image)
  !   write (*, *) "[I] - image:",image

  end subroutine generic_source_data

  ! Close the shared-object 
  ! 
  subroutine generic_source_close(handle)
    use iso_c_binding
    use iso_c_utilities
    use dlfcn
    implicit none    
    type(c_ptr)                        :: handle
    write (*, *) "[I] - generic_source_close <", handle,">"

    ! now close the dl:
    status=dlclose(handle)
    if(status/=0) then
       write(*,*) "error in dlclose: ", c_f_string(dlerror())
       stop
    end if
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
  logical                         :: error_flag
  integer                         :: return_value=0

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
  write (*, *) "[I] - External_source_flag:", external_source_flag
 
  if (external_source_flag) then
     write (*, *) "[I] - Loading shared-object"
     detector      = 'my_detector'
     template_name = 'my_name_template'
     return_value = generic_source_open(handle, detector, template_name)
     call generic_source_data(handle, 10, 10, image, error_flag)
     call generic_source_close(handle)
  else
     write (*, *) "[I] - No shared-object required"
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
