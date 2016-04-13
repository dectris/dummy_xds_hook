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

module global_source
  use iso_c_binding
  implicit none
  type(c_ptr)                     :: handle
  integer                         :: number
  character(kind=c_char,len=1024) :: dll_name
  integer(c_int)                  :: status

  ! Define a type for an array of pointers
  type pp
    type(c_ptr), dimension(:), pointer :: p
  end type pp

  !type(pp), dimension(:), allocatable :: array
  integer :: i

  public                          :: global_source_open !, global_source_header, global_source_data, global_source_clone


contains


  ! Open the shared-object 
  ! 
  subroutine global_source_open()
    use iso_c_binding
    use iso_c_utilities
    use dlfcn
    implicit none    

    !type(pp), dimension(256) :: handle_array
    integer (c_int32_t), dimension () :: image

    character(kind=c_char,len=1024)    :: sub_name="external_image_read"
    type(c_ptr)                        :: handle=c_null_ptr
    type(c_funptr)                     :: funptr=c_null_funptr

    ! the dynamic subroutine has a simple interface:
    abstract interface
       subroutine external_image_read(image) bind(C)
         use iso_c_binding
         integer (c_int32_t), dimension (1) :: image
         !real(c_double), value :: x
       end subroutine external_image_read
    end interface
    procedure(external_image_read), pointer :: dll_sub ! dynamically-linked procedure

    !do i = 1, size(array)
    !   allocate(array(i)%p(i))
    !end do

    dll_name="libDectrisSource.so"
    ! Open the DL:
    handle=dlopen(trim(dll_name)//C_NULL_CHAR, IOR(RTLD_NOW, RTLD_GLOBAL))
    ! The use of IOR is not really proper...wait till Fortran 2008  
    if(.not.c_associated(handle)) then
       write(*,*) "error in dlopen: ", c_f_string(dlerror())
       stop
    end if

    ! Find the subroutine in the DL:
    funptr=DLSym(handle,trim(sub_name)//C_NULL_CHAR)
    if(.not.c_associated(funptr)) then
       write(*,*) "error in dlsym: ", c_f_string(dlerror())
       stop
    end if


    !write (*, *) "[I] - global_source_open", handle
  end subroutine global_source_open


  ! Close the shared-object 
  ! 
  subroutine global_source_close()
    use iso_c_binding
    use iso_c_utilities
    use dlfcn
    implicit none    
    write (*, *) "[I] - global_source_close"

    ! now close the dl:
    status=dlclose(handle)
    if(status/=0) then
       write(*,*) "error in dlclose: ", c_f_string(dlerror())
       stop
    end if
  end subroutine global_source_close

end module global_source




program image_consumer
  use iso_c_binding
  use global_source

  implicit none
  !integer, parameter :: MAX_IMAGE_SIZE = 9
  !integer (c_int32_t), dimension (0:MAX_IMAGE_SIZE) :: image


  integer::number_of_arguments,cptArg
  logical::external_source_flag=.FALSE.
  character(len=20)::name

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
     call global_source_open()
     call global_source_close()
  else
     write (*, *) "[I] - No shared-object required"
  endif

  stop
  
end program image_consumer



!
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
