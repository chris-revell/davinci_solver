!CK Revell, March 2016
!Module containing function to update upper boundary velocity in da vinci solver

module davinci_1_upperboundary

  use davinci_0_arrays

  implicit none

contains

  real function upperboundary(t)

    integer, intent(in) :: t

    upperboundary = 0.0

  end function upperboundary

end module davinci_1_upperboundary
