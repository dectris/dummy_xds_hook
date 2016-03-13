!     Dummy image receiver
! 
!
!
!
!
      program image_consumer
      use iso_c_binding
      implicit none
 
      interface c_interface
      
      subroutine external_image_provider (image) bind (C, name = "")
      use iso_c_binding
      implicit none
      integer (c_int32_t), intent (out), dimension (*) :: image
      end subroutine external_image_provider
      
      end interface c_interface
      integer (c_int32_t), dimension (0:3) :: image
      


      integer::number_of_arguments,cptArg
      logical::external_image_provider_flag=.FALSE.
      character(len=20)::name

      number_of_arguments=command_argument_count()
      call get_command_argument(cptArg,name)

      if(number_of_arguments .eq. 1 ) then
         select case(adjustl(name))
         case("--ext")
            external_image_provider_flag=.TRUE.
         case default
            external_image_provider_flag=.FALSE.
         end select  
      endif
      write (*, *) "external_image_provider_flag:",external_image_provider_flag

      
    

      
      call image_provider (image)
      write (*, *) "C-loaded image:",image
      stop
      
      end program image_consumer
      
!
! Internal Image Provider
!
      subroutine internal_image_provider (image) 
      implicit none
      integer image

!      INT32, intent (out), dimension (*) :: image
      
!      image(1) = 1
!      image(2) = 2
!      image(3) = 3
      
      end subroutine internal_image_provider
