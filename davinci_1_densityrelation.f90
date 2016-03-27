!CK Revell, March 2016
!Module containining density relation for da vinci solver

module davinci_1_densityrelation

  use davinci_0_arrays
  use davinci_0_variables

  implicit none

contains

  REAL FUNCTION DensityRelation(n)
    INTEGER, intent(in) :: n
    REAL :: z
    z=REAL(n-1)*d
    DensityRelation = K*EXP(alpha*g*(H-z))
  END FUNCTION DensityRelation

end module davinci_1_densityrelation
