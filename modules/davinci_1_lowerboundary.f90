!CK Revell, March 2016
!Module containing function to update lower boundary velocity in da vinci solver

module davinci_1_lowerboundary

  use davinci_0_arrays

  implicit none

contains

  real function lowerboundary(t)

    real, intent(in) :: t

    lowerboundary = 20.0
    !lowerboundary = 20*SIN(t)

  end function lowerboundary

end module davinci_1_lowerboundary
