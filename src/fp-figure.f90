module fp_figure
  use fig_drawing
  use fig_bitmap
  use fig_svg
  implicit none

  integer, parameter :: DEFAULT_WIDTH = 600
  integer, parameter :: DEFAULT_HEIGHT = 450

  type, abstract :: Figure
    character(len=100) :: title
    integer :: width = DEFAULT_WIDTH
    integer :: height = DEFAULT_HEIGHT
    type(drawing) :: draw

  contains
    procedure(build), deferred :: build
    procedure(clear), deferred :: clear
    procedure :: save_to_png => figure_save_svg
    procedure :: save_to_svg => figure_save_png
  end type Figure

  abstract interface
    subroutine build(this)
      import :: Figure
      class(Figure), intent(inout) :: this
    end subroutine build

    subroutine clear(this)
      import :: Figure
      class(Figure), intent(inout) :: this
    end subroutine clear
  end interface

contains

  subroutine figure_save_svg(this, filepath)
    class(Figure), intent(in) :: this
    character(len=*), intent(in) :: filepath
    type(svg_canvas) :: svg_canva

    call svg_canva%init(this%width,this%height,filepath//this%title)
    call svg_canva%apply_shapes(this%draw)
    call svg_canva%save_to_svg()
    call svg_canva%destroy()
  end subroutine figure_save_svg

  subroutine figure_save_png(this, filepath)
    class(Figure), intent(in) :: this
    character(len=*), intent(in) :: filepath
    type(bitmap_canvas) :: bitmap_canva

    call bitmap_canva%init(this%width,this%height,filepath//this%title)
    call bitmap_canva%apply_shapes(this%draw)
    call bitmap_canva%save_to_png()
    call bitmap_canva%destroy()
  end subroutine figure_save_png
end module fp_figure
