!CK Revell, March 2016
!Module containining density relation for da vinci solver
!Relates layer density to height

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


!REAL FUNCTION DensityRelation(n, v)
!  INTEGER n
!  REAL, DIMENSION(TotalLayers+2), INTENT(IN) :: v

!  REAL Sum
!  Sum = 0
!  DO i=n, (TotalLayers+1)
!    Sum = Sum + ((VelocityRelation(v(i+1))-VelocityRelation(v(i)))/d)*EXP(alpha*g*n*d)
!  END DO

!  DensityRelation = K*EXP(-alpha*g*n*d)*Sum

!END FUNCTION


!REAL FUNCTION VelocityRelation(v)
!  REAL v
!  VelocityRelation = -v/25000
!END FUNCTION		
