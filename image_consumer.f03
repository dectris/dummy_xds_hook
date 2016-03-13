!     Dummy image receiver
! 
program image_consumer
  use iso_c_binding
  implicit none
  integer, parameter :: MAX_IMAGE_SIZE = 9
  integer (c_int32_t), dimension (0:MAX_IMAGE_SIZE) :: image
  
  

  integer::number_of_arguments,cptArg
  logical::external_image_provider_flag=.FALSE.
  character(len=20)::name
  
  number_of_arguments=command_argument_count()

  if(number_of_arguments == 1 ) then
     call get_command_argument(1,name)
          select case(adjustl(name))
     case("--ext")
        external_image_provider_flag=.TRUE.
     case default
        external_image_provider_flag=.FALSE.
     end select
  endif
  write (*, *) "[I] - External_image_provider_flag:",external_image_provider_flag
  
  
  

  if (external_image_provider_flag) then
     call external_image_read (image)
     write (*, *) "[I] - C-loaded image:",image
  else
     call internal_image_read (image)
     write (*, *) "[I] - Fortran-loaded image:",image
  endif

  stop
  
end program image_consumer

!
! Internal Image Read
!
subroutine internal_image_read (image) 
  use iso_c_binding
  implicit none
  integer (c_int32_t), dimension (*) :: image
  integer :: pixel

  do pixel = 0, 10, 1
     image(pixel) = pixel
  enddo
  
end subroutine internal_image_read


!
! External Image Read (thought c/c++ code)
!
subroutine external_image_read (image) 
  use iso_c_binding
  implicit none
  
  interface c_interface
     
     subroutine external_image_provider (image) bind (C, name = "")
       use iso_c_binding
       implicit none
       integer (c_int32_t), intent (out), dimension (*) :: image
     end subroutine external_image_provider

  end interface c_interface
  integer (c_int32_t), dimension (*) :: image
  call image_provider (image)

end subroutine external_image_read
