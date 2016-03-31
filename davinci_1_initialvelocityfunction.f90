!CK Revell, March 2016
!Module containing InitialVelocityFunction of da Vinci fluid solver
!Function describing initial velocity profile of fluid.

module davinci_1_initialvelocityfunction

  use davinci_0_arrays
  use davinci_0_variables

  implicit none

contains

  REAL FUNCTION InitialVelocityFunction(n, FunctionChoice)

    INTEGER n
    INTEGER FunctionChoice

    IF (FunctionChoice.EQ.1) THEN !At rest
      InitialVelocityFunction = 10.0

!    ELSE IF (FunctionChoice.EQ.2) THEN	!Linear
!      InitialVelocityFunction = v_bottom + (n-1)*(v_top-v_bottom)/(TotalLayers+1)

!    ELSE IF (FunctionChoice.EQ.3) THEN	!Parabolic with maximum in middle
!      InitialVelocityFunction = (v_bottom + (TotalLayers+1)*(TotalLayers+1)/4 - ((n-1)-(TotalLayers+1)/2)*((n-1)-(TotalLayers+1)/2))

!    ELSE IF (FunctionChoice.EQ.4) THEN !Parabolic with maxima at edges
!      InitialVelocityFunction = v_bottom + ((n-1)-(TotalLayers+1)/2)*((n-1)-(TotalLayers+1)/2)

!    ELSE IF (FunctionChoice.EQ.5) THEN !2 plugs, upper at speed of upper boundary
!      IF (n.LT.((TotalLayers+2)/3)) THEN
!        InitialVelocityFunction = v_bottom
!      ELSE IF (n.GT.(2*(TotalLayers+2)/3)) THEN
!        InitialVelocityFunction = v_top
!      ELSE
!        InitialVelocityFunction = v_bottom + (n-NINT(REAL((TotalLayers+2)/3)))*(3*(v_top-v_bottom)/(TotalLayers+2))
!      END IF

!    ELSE IF (FunctionChoice.EQ.6) THEN	!2 plugs, upper below speed of upper boundary
!      IF (n.LT.((TotalLayers+2)/3)) THEN
!        InitialVelocityFunction = v_bottom
!      ELSE IF (n.GT.(2*(TotalLayers+2)/3)) THEN
!        InitialVelocityFunction = 0.75*v_top
!      ELSE
!        InitialVelocityFunction = v_bottom + (n-NINT(REAL((TotalLayers+2)/3)))*(3*(0.75*v_top-v_bottom)/(TotalLayers+2))
!      END IF

    END IF

  END FUNCTION InitialVelocityFunction

end module davinci_1_initialvelocityfunction
