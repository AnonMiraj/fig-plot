program chess_checker
    use fp_frame
    use fp_2dplot
    use fig_drawing
    use fig_rgb
    use fig_shapes
    use fig_rgb_color_constants
    use fig_test
    implicit none
    integer, parameter :: WIDTH = 650
    integer, parameter :: HEIGHT = 450
    integer :: x, y,n_size,i
    character(len=:), allocatable  :: file_name

    type(svg_canvas) :: svg_canva
    type(bitmap_canvas) :: bitmap_canva
    type(Frame) :: frm
    type(Plot2D) :: pp
    type(drawing) :: checker

    real,dimension(:),allocatable :: x_data, y_data
    real :: pi
    FIG_ABSOLUTE_COORDINATES = .true.
    file_name = "tri_plot"
    n_size = 500
    allocate(x_data(n_size))
    allocate(y_data(n_size))
    pi = 4.0 * atan(1.0)

    do i = 1, n_size
        x_data(i) = (i-1) * 2.0 * pi / (n_size - 1)
        y_data(i) = cos(x_data(i))
    end do

    call pp%plot(x_data, y_data, n_size)

    do i = 1, n_size
        x_data(i) = (i-1) * 2.0 * pi / (n_size - 1)
        y_data(i) = sin(x_data(i))
    end do

    call pp%plot(x_data, y_data, n_size)




    call pp%build
    pp%title = trim(file_name)
    pp%draw%background=FIG_COLOR_WHITE

    call pp%save_to_png("")
    call pp%save_to_svg("")

end program chess_checker

