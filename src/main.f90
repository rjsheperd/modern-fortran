program main
  implicit none


  integer :: i, n
  integer, parameter :: gridsize = 100
  integer, parameter :: num_time_steps = 100

  real, parameter :: dt = 1, dx = 1, c = 1

  real :: h(gridsize), dh(gridsize)

  integer, parameter :: icenter = 25
  real, parameter :: decay = 0.02

  print *, "Beginning simulation..."

  do concurrent (i = 1:gridsize)
    h(i) = exp(-decay * (i - icenter)**2)
  end do

  time_loop: do n = 1, num_time_steps
    dh(1) = h(1) - h(gridsize)
    do i = 2, gridsize
      dh(i) = h(i) - h(i - 1)
    end do

    do concurrent(i = 1:gridsize)
      h(i) = h(i) - c * dh(i) / dx * dt
    end do

    if ( mod(n, 10) == 0 ) print *, n, h
  end do time_loop

end program
