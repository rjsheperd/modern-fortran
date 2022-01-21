program main
  implicit none

  integer :: n
  integer, parameter :: gridsize = 100
  integer, parameter :: num_time_steps = 100

  real, parameter :: dt = 1, dx = 1, c = 1

  real :: h(gridsize), dh(gridsize)

  print *, "Beginning simulation..."

  call set_gaussian(h, 25, 0.02)

  time_loop: do n = 1, num_time_steps
    h  = h - c * diff(h) / dx * dt
    if ( mod(n, 10) == 0 ) print *, n, h
  end do time_loop

contains

  function diff(x) result(dx)
    real, intent(in) :: x(:)
    real :: dx(size(x))
    integer :: im
    im = size(x)
    dx(1) = x(1) - x(im)
    dx(2:) = x(2:) - x(1:im-1)
  end function diff

  subroutine set_gaussian(x, icenter, decay)
    real, intent(in out) :: x(:)
    integer, intent(in) :: icenter
    real, intent(in) :: decay
    integer :: i

    do concurrent (i = 1:size(x))
      x(i) = exp(-decay * (i - icenter)**2)
    end do
  end subroutine set_gaussian

end program
