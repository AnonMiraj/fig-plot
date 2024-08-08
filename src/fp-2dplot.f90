
module fp_2dplot
  use fp_figure
  use fp_utils
  use fp_frame
  use fig_rgb
  use fig_shapes
  use fig_poly
  implicit none

 
  type(RGB), dimension(8) :: color_palette


  type, extends(Figure) :: Plot2D
    private
    logical :: m_hold = .true.
    character(len=100) :: m_x_label
    character(len=100) :: m_y_label

    type(data_series), allocatable :: m_numeric_data(:)
    type(interval):: m_x_data_range
    type(interval):: m_y_data_range
    type(interval):: m_x_range
    type(interval):: m_y_range
    type(ColorSelector):: m_color_selector
    real :: m_zoom_x = 1.0
    real :: m_zoom_y = 1.0

    real :: m_frame_x, m_frame_y, m_frame_w, m_frame_h

    logical :: m_grid_enable = .false.
    real, allocatable :: m_x_markers(:)
    real, allocatable :: m_y_markers(:)
    real, allocatable :: m_x_custom_markers(:)
    real, allocatable :: m_y_custom_markers(:)

  contains
    procedure :: build => plot2d_build
    procedure :: clear => plot2d_clear

    procedure :: plot => plot2d_plot
    ! procedure :: scatter1 => plot2d_scatter1

    procedure :: CalculateNumericFrame
    ! procedure :: set_legend => plot2d_set_legend


    procedure :: calculate_frame => plot2d_calculate_frame
    procedure :: draw_frame => plot2d_draw_frame
    procedure :: draw_data => plot2d_draw_data
    ! procedure :: draw_title => plot2d_draw_title
    ! procedure :: draw_labels => plot2d_draw_labels
    ! procedure :: draw_legend => plot2d_draw_legend
  end type Plot2D

  type :: style
    type(RGB) :: color = FIG_COLOR_RED
    real :: stroke = 1.0
    character(len=100) :: dash_array = ""
    logical :: scatter = .false.
  end type style

  type :: data_series
    real, allocatable :: x(:)
    real, allocatable :: y(:)
    type(style) :: style
  end type data_series

  real, parameter :: FRAME_TOP_MARGIN_REL = 0.10
  real, parameter :: FRAME_BOTTOM_MARGIN_REL = 0.12
  real, parameter :: FRAME_LEFT_MARGIN_REL = 0.15
  real, parameter :: FRAME_RIGHT_MARGIN_REL = 0.05

  real :: m_axis_font_size

  real, parameter :: PIXELS_PER_X_MARKER = 80.0
  real, parameter :: PIXELS_PER_Y_MARKER = 80.0
  real, parameter :: MARKER_LENGTH = 5.0
  integer, parameter :: MAX_NUM_Y_MARKERS = 5
  integer, parameter :: MAX_NUM_X_MARKERS = 10

  real, parameter :: BASE_TITLE_FONT_SIZE = 20.0
  real, parameter :: BASE_AXIS_FONT_SIZE = 11.0

  real, parameter :: LEGEND_MARGIN = 5.0

contains

  subroutine plot2d_build(this)
    class(Plot2D), intent(inout) :: this
    integer ::  i 

    call this%draw%init()
    call this%calculate_frame()
    call this%draw_frame()
    do i=1 , size(this%m_numeric_data),1
      call this%draw_data(this%m_numeric_data(i))
    end do 

  end subroutine plot2d_build

  subroutine plot2d_clear(this)
    class(Plot2D), intent(inout) :: this
  end subroutine plot2d_clear



  subroutine plot2d_plot(this, x_data, y_data, n_size)
      class(Plot2D), intent(inout) :: this
      real, intent(in) :: x_data(:), y_data(:)
      integer, intent(in) :: n_size
      integer :: current_size, new_size
      type(data_series), allocatable :: temp(:)

      if (allocated(this%m_numeric_data)) then
          current_size = size(this%m_numeric_data)
      else
          current_size = 0
      endif
      new_size = current_size + 1

      allocate(temp(new_size))

      if (current_size > 0) then
          temp(1:current_size) = this%m_numeric_data
      endif

      allocate(temp(new_size)%x(n_size))
      allocate(temp(new_size)%y(n_size))
      temp(new_size)%x = x_data
      temp(new_size)%y = y_data

      call move_alloc(temp, this%m_numeric_data)
  end subroutine plot2d_plot

  ! subroutine plot2d_scatter1(this, x_data, y_data, color, radius)
  !   class(Plot2D), intent(inout) :: this
  !   real, intent(in) :: x_data(:), y_data(:)
  !   type(color), intent(in) :: color
  !   real, optional, intent(in) :: radius
  !   ! Implement scatter logic here
  ! end subroutine plot2d_scatter1
  !

  subroutine plot2d_calculate_frame(this)
    class(Plot2D), intent(inout) :: this
    integer :: num_x_markers, num_y_markers

    this%m_frame_x = this%width *  FRAME_LEFT_MARGIN_REL
    this%m_frame_y = this%height * FRAME_TOP_MARGIN_REL
    this%m_frame_w = this%width *  &
      (1.0 - FRAME_LEFT_MARGIN_REL - FRAME_RIGHT_MARGIN_REL)
    this%m_frame_h = this%height * &
      (1.0 - FRAME_TOP_MARGIN_REL - FRAME_BOTTOM_MARGIN_REL)
    call this%CalculateNumericFrame()

    num_x_markers = min(MAX_NUM_X_MARKERS, int(this%m_frame_w / PIXELS_PER_X_MARKER))
    call PartitionRange(this%m_x_data_range, num_x_markers, this%m_x_markers)
    
    num_y_markers = min(MAX_NUM_Y_MARKERS, int(this%m_frame_h / PIXELS_PER_Y_MARKER))
    call PartitionRange(this%m_y_data_range, num_y_markers, this%m_y_markers)

    m_axis_font_size=12

  end subroutine plot2d_calculate_frame
  subroutine CalculateNumericFrame(this)
    class(Plot2D), intent(inout) :: this

    real :: min_x, max_x, min_y, max_y
    real :: x, y, zoom_x, zoom_y
    integer :: i, j, sz
    type(data_series) :: plot

    min_x = huge(1.0)
    max_x = -huge(1.0)
    min_y = huge(1.0)
    max_y = -huge(1.0)



    print * , size(this%m_numeric_data)
    do i = 1, size(this%m_numeric_data)
      plot = this%m_numeric_data(i)
      sz = size(plot%x)
      do j = 1, sz
          x = plot%x(j)
          y = plot%y(j)
          
              min_x = min(x, min_x)
              max_x = max(x, max_x)
              min_y = min(y, min_y)
              max_y = max(y, max_y)
      end do
    end do

    this%m_x_data_range%min=min_x
    this%m_x_data_range%max=max_x
    this%m_y_data_range%min=min_y
    this%m_y_data_range%max=max_y

    this%m_zoom_x = abs(real(this%m_frame_w) / real(this%m_x_data_range%max - this%m_x_data_range%min))
    this%m_zoom_y = abs(real(this%m_frame_h) / real(this%m_y_data_range%max - this%m_y_data_range%min))

  end subroutine CalculateNumericFrame

  subroutine plot2d_draw_frame(this)
    class(Plot2D), intent(inout) :: this

    type(Frame) :: frame_
    type(Marker), allocatable :: left_markers(:)
    type(Marker), allocatable :: bottom_markers(:)
    real :: mark
    real :: x, y
    real :: tx, ty
    integer :: i, n_left, n_bottom
    logical :: insert_left, insert_bottom

    call frame_%init(this%m_frame_w, this%m_frame_h, .false.)
    print * , this%m_frame_w, this%m_frame_h

    n_left = size(this%m_y_markers)
    allocate(left_markers(n_left))

    n_bottom = size(this%m_x_markers)
    allocate(bottom_markers(n_bottom))

    do i= 1, n_left
        mark = this%m_y_markers(i)
        if (mark >= this%m_y_data_range%min .and. mark <= this%m_y_data_range%max) then
            call TranslateToFrame(this, 0.0, mark, tx, ty)
            left_markers(i)%pos = ty
            write(left_markers(i)%txt, '(F6.2)') mark
        end if
    end do
    ! do i= 1, n_left
    !     mark = this%m_y_custom_markers(i)
    !     if (mark >= this%m_y_range%min .and. mark <= this%m_y_range%max) then
    !         call TranslateToFrame(this, 0.0, mark, tx, ty)
    !         left_markers(i)%pos = ty
    !         write(left_markers(i)%txt, '(F6.2)') mark
    !     end if
    ! end do
    if (i < n_left) then
        left_markers = left_markers(:i)
    end if

    do i = 1, size(left_markers)
        call frame_%add_left_marker(left_markers(i)%pos,left_markers(i)%txt)
    end do

    do i = 1 , n_bottom
        mark = this%m_x_markers(i)
        if (mark >= this%m_x_data_range%min .and. mark <= this%m_x_data_range%max) then
            call TranslateToFrame(this, mark, 0.0, tx, ty)
            bottom_markers(i)%pos = tx
            write(bottom_markers(i)%txt, '(F6.2)') mark
        end if
    end do
    ! do i=1 , n_bottom
    !     mark = this%m_x_custom_markers(i)
    !     if (mark >= this%m_x_range%min .and. mark <= this%m_x_range%max) then
    !         call TranslateToFrame(this, mark, 0.0, tx, ty)
    !         bottom_markers(i)%pos = tx
        !     write(bottom_markers(i)%txt, '(F6.2)') mark
        ! end if
    ! end do
    if (i < n_bottom) then
        bottom_markers = bottom_markers(:i)
    end if
    do i = 1, size(bottom_markers)
        call frame_%add_bottom_marker(bottom_markers(i)%pos,bottom_markers(i)%txt)
    end do
    call frame_%draw(this%draw, this%m_frame_x, this%m_frame_y)

  end subroutine plot2d_draw_frame
    
  subroutine TranslateToFrame(this, x, y, tx, ty)
      class(Plot2D), intent(in) :: this
      real, intent(in) :: x, y
      real, intent(out) :: tx, ty

      tx = this%m_zoom_x * (x - this%m_x_data_range%min) 
      ty = this%m_frame_h - (this%m_zoom_y * (y - this%m_y_data_range%min)) 

  end subroutine TranslateToFrame

  subroutine plot2d_draw_data(this,plot)
    class(Plot2D), intent(inout) :: this
    class(data_series), intent(in) :: plot
    type(polyline) :: pl
    real,allocatable :: x_points(:),y_points(:)
    real :: tx, ty
    integer :: sz,i

    sz=size(plot%x)
    allocate(x_points(sz))
    allocate(y_points(sz))
    do i = 1, sz,1
      call TranslateToFrame(this ,plot%x(i), plot%y(i), tx, ty)
      x_points(i)=tx + this%m_frame_x
      y_points(i)=ty + this%m_frame_y
    end do

    call pl%add_points(x_points, y_points, sz)

    pl%stroke_color = this%m_color_selector%next_color()
    call this%draw%add_shape(pl)
  end subroutine plot2d_draw_data

end module fp_2dplot
