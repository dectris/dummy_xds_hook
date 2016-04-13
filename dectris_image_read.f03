!SUBROUTINE MySub(x) BIND(C,NAME="MySub")
!   USE ISO_C_BINDING
!   REAL(C_DOUBLE), VALUE :: x
!   WRITE(*,*) "MySub: x=",x
!END SUBROUTINE   

subroutine external_image_read (image) bind(C,name="external_image_read")
  use iso_c_binding
  implicit none
  integer (c_int32_t), dimension (*) :: image
  integer :: pixel

  do pixel = 0, 10, 1
     image(pixel) = 42
  enddo
  
end subroutine external_image_read
