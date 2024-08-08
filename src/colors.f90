module fp_utils

  use fig_rgb
  implicit none
  type, public :: ColorSelector
    type(RGB), dimension(7) ::  m_table = &
                  [ RGB(238, 119, 51,255), &
                    RGB(0, 119, 187,255), &
                    RGB(51, 187, 238,255), &
                    RGB(238, 51, 119,255), &
                    RGB(204, 51, 17,255), &
                    RGB(0, 153, 136,255), &
                    RGB(187, 187, 187,255) ]
    integer :: index = 1
  contains
    procedure :: next_color
  end type ColorSelector

  type :: interval
    real :: min
    real :: max
  end type interval
contains 
  
  function next_color(this) result(color)
    class(ColorSelector), intent(inout) :: this
    type(RGB) :: color

    color = this%m_table(this%index)
    this%index = mod(this%index, size(this%m_table)) + 1
  end function next_color

  subroutine PartitionRange(range, num_markers, markers)

      type(interval), intent(in) :: range
      integer, intent(in) :: num_markers
      real, dimension(:), allocatable, intent(out) :: markers

      integer :: i
      real :: step

      step = (range%max - range%min) / num_markers
      allocate(markers(num_markers+1))

      do i = 1, num_markers+1
          markers(i) = range%min + step * (i - 1)
      end do

  end subroutine PartitionRange


end module fp_utils
