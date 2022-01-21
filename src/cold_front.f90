program cold_front
  implicit none

  integer :: n
  real :: dt(8)

  dt = [6, 12, 18, 24, 30, 36, 42, 48]

  print *, "Temperatures:", cold_front_temp(12., 24., 20., 960., dt)

contains

  pure elemental real function cold_front_temp(temp1, temp2, c, dx, dt) result(res)
    real, intent(in) :: temp1, temp2, dt, dx, c
    res = temp2 - c * (temp2 - temp1) / dx * dt
  end function cold_front_temp

end program
