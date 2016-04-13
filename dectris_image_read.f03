!
! Dummy fortran external reader function
!
subroutine external_image_read (image) bind(C,name="external_image_read")
  use iso_c_binding
  implicit none
  integer (c_int32_t), dimension (*) :: image
  integer :: pixel

  do pixel = 0, 10, 1
     image(pixel) = 42
  enddo
  
end subroutine external_image_read
