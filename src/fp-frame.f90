module fp_frame
  use fig_drawing
  implicit none

  integer, parameter :: FULL_FRAME = 1
  integer, parameter :: AXES_ONLY = 2
  type(RGB) ::  Border_color=RGB(128,128,128,255)
  type:: Frame
    real :: width , height
    logical :: grid_enable = .false.
    integer :: frame_type = FULL_FRAME
    real :: axis_font_size = 15.0
    real :: MARKER_LENGTH = 5.0
    character(9) :: TEXT_FONT= 'monospace'



    type(Marker), allocatable :: left_markers(:)
    type(Marker), allocatable :: bottom_markers(:)
  contains
    procedure :: init => frame_init
    procedure :: draw => frame_draw
    procedure :: draw_full_frame
    procedure :: draw_axes_only
    procedure :: draw_axes
    procedure :: add_left_marker => frame_add_left_marker
    procedure :: add_bottom_marker => frame_add_bottom_marker
  end type Frame

  type :: Marker
    real :: pos
    character(len=100) :: txt
  end type Marker


contains


  subroutine frame_init(this, w, h, enable_grid)
    class(Frame), intent(inout) :: this
    real, intent(in) :: w, h
    logical, intent(in) :: enable_grid

    this%width = w
    this%height = h
    this%grid_enable = enable_grid
  end subroutine frame_init


  subroutine frame_draw(this, frame_drawing, x, y)

    class(Frame), intent(in) :: this
    type(drawing), intent(inout) :: frame_drawing
    real, intent(in) :: x, y
    
    call this%draw_axes(frame_drawing, x, y)
    if (this%frame_type == FULL_FRAME) then
    call this%draw_full_frame(frame_drawing, x, y)
    else
      call this%draw_axes_only(frame_drawing, x, y)
    end if
  end subroutine frame_draw

  subroutine draw_full_frame(this, frame_drawing, x, y)
    class(Frame), intent(in) :: this
    type(drawing), intent(inout) :: frame_drawing
    real, intent(in) :: x, y
    type(rectangle) :: rect

    rect%upper_left%x=x
    rect%upper_left%y=y
    rect%width=this%width
    rect%height=this%height
    rect%stroke_color = Border_color
    
    call frame_drawing%add_shape(rect)


  end subroutine draw_full_frame

  subroutine draw_axes_only(this, frame_drawing, x, y)
    class(Frame), intent(in) :: this
    type(drawing), intent(inout) :: frame_drawing
    real, intent(in) :: x, y

    type(line) :: l

    l%p1%x = x
    l%p1%y = y
    l%p2%x = x
    l%p2%y = y + this%height
    l%stroke_color = Border_color
    
    call frame_drawing%add_shape(l)

    l%p1%y = y + this%height
    l%p2%x = x + this%width

    call frame_drawing%add_shape(l)

  end subroutine draw_axes_only

  subroutine draw_axes(this, frame_drawing, x, y)
    class(Frame), intent(in) :: this
    type(drawing), intent(inout) :: frame_drawing
    real, intent(in) :: x, y

    real :: font_em
    integer :: i
    type(Marker) :: mark
    type(line) :: marker_line, grid_line
    type(text) :: text_node

    font_em = this%axis_font_size / 12.0

    if (allocated(this%left_markers)) then
      do i = 1, size(this%left_markers)
        mark = this%left_markers(i)
        marker_line%p1%x =  x
        marker_line%p1%y =  y + mark%pos
        marker_line%p2%x =  x - this%MARKER_LENGTH
        marker_line%p2%y =  y + mark%pos
        marker_line%stroke_color = Border_color
        call frame_drawing%add_shape(marker_line)
        text_node%p%x=x-2*this%MARKER_LENGTH - 60
        text_node%p%y=y+mark%pos + font_em * 4.0
        text_node%content= mark%txt
        text_node%size=this%axis_font_size
        text_node%font_family= this%TEXT_FONT
        text_node%stroke_color = FIG_COLOR_BLACK
        call frame_drawing%add_shape(text_node)

        if (this%grid_enable) then
          grid_line%p1%x =  x
          grid_line%p1%y =  y + mark%pos
          grid_line%p2%x =  x + this%width
          grid_line%p2%y =  y + mark%pos
          grid_line%stroke_color = Border_color
          grid_line%stroke_width = 0.75
          !! TODO ADD DASHED lines to fig
          call frame_drawing%add_shape(grid_line)
        end if
      end do
    end if 
    
    if (allocated(this%bottom_markers)) then
      do i = 1, size(this%bottom_markers)
        mark = this%bottom_markers(i)
        marker_line%p1%x =  x + mark%pos
        marker_line%p1%y =  y + this%height
        marker_line%p2%x =  x + mark%pos
        marker_line%p2%y =  y + this%height + this%MARKER_LENGTH
        marker_line%stroke_color = Border_color
        call frame_drawing%add_shape(marker_line)
        text_node%p%x=x+mark%pos - 30
        text_node%p%y=y+ this%height + this%MARKER_LENGTH + font_em * 16.0 
        text_node%content= mark%txt
        text_node%size=this%axis_font_size -1
        text_node%font_family= this%TEXT_FONT 
        text_node%stroke_color = FIG_COLOR_BLACK
        call frame_drawing%add_shape(text_node)

        if (this%grid_enable) then
          grid_line%p1%x =  x + mark%pos
          grid_line%p1%y =  y
          grid_line%p2%x =  x + mark%pos
          grid_line%p2%y =  y + this%height
          grid_line%stroke_color = Border_color
          grid_line%stroke_width = 0.75
          !! TODO ADD DASHED lines to fig
          call frame_drawing%add_shape(grid_line)
        end if
      end do
    end if

  end subroutine draw_axes
  subroutine frame_add_left_marker(this, pos, txt)
    class(Frame), intent(inout) :: this
    real, intent(in) :: pos
    character(len=*), intent(in) :: txt

    call add_marker(this%left_markers, pos, txt)
  end subroutine frame_add_left_marker

  subroutine frame_add_bottom_marker(this, pos, txt)
    class(Frame), intent(inout) :: this
    real, intent(in) :: pos
    character(len=*), intent(in) :: txt

    call add_marker(this%bottom_markers, pos, txt)
  end subroutine frame_add_bottom_marker

  subroutine add_marker(marker_array, pos, txt)
    type(Marker), allocatable, intent(inout) :: marker_array(:)
    real, intent(in) :: pos
    character(len=*), intent(in) :: txt

    type(Marker) :: new_marker
    type(Marker), allocatable :: temp_array(:)
    integer :: n

    new_marker%pos = pos
    new_marker%txt = txt
    print *,new_marker
    if (.not. allocated(marker_array)) then
      allocate(marker_array(1))
      marker_array(1) = new_marker
    else
      n = size(marker_array)
      allocate(temp_array(n + 1))
      temp_array(1:n) = marker_array
      temp_array(n + 1) = new_marker
      call move_alloc(temp_array, marker_array)
    end if
  end subroutine add_marker
end module fp_frame
