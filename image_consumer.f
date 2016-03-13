*     Dummy image receiver
* 
*
*
*
*
      program image_consumer
      use iso_c_binding

      implicit none
      integer::number_of_arguments,cptArg
      logical::external_image_provider_flag=.FALSE.

      number_of_arguments=command_argument_count()
      call get_command_argument(cptArg,name)

      if(narg .eq. 1 ) then
         select case(adjustl(name))
         
         case("--ext")
            external_image_provider_flag=.TRUE.
         case default
            external_image_provider_flag=.FALSE.
         end select  
      endif
      
      interface c_interface
      
      subroutine external_image_provider (image) bind (C, name = "")
      use iso_c_binding
      implicit none
      integer (c_int32_t), intent (out), dimension (*) :: image
      end subroutine external_image_provider
      
      end interface c_interface
      

      subroutine internal_image_provider (image) 
      implicit none
      integer (c_int32_t), intent (out), dimension (*) :: image
      
      image[0] = 1
      image[1] = 2
      image[2] = 3
      
      end subroutine internal_image_provider
      
    

      integer (c_int32_t), dimension (0:3) :: image
      
      call image_provider (image)
      write (*, *) "C-loaded image:",image
      stop
      
      end program image_consumer
      
