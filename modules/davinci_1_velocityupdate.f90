!CK Revell, March 2016
!Module containing subroutine to update layer velocities in the da vinci fluid solver

module davinci_1_velocityupdate

  use davinci_0_variables
  use davinci_0_arrays
  use davinci_1_plugtreatment
  use davinci_1_lowerboundary
  use davinci_1_upperboundary

  implicit none

contains

  subroutine velocityupdate

    !Loop over all layers, excluding upper and lower boundaries.
    DO n=2, TotalLayers+1

      !Compute velocity differences at layer boundaries
      dv_lower  = v(n)-v(n-1)
      dv_upper  = v(n+1)-v(n)

      !Compute signs of upper and lower velocity differences
      sign_lower = SIGN(1.0,dv_lower)
      sign_upper = SIGN(1.0,dv_upper)

      !Compute normal force on upper edge of layer n
      MassAboveLayer=0
      Do i=(n+1), TotalLayers+1
        MassAboveLayer = MassAboveLayer + rho(i)*d
      END DO
      P = P_0 + MassAboveLayer*g    !External pressure plus sum of masses of layers above, times gravitational constant

!Everything below needs to be reassessed
!******************************************************************************

      IF (PlugArray(n)) THEN
        !PlugArray(n) = TRUE means layer is part of a plug and its acceleration will be calculated later in the plug treatment routine.
        !Do nothing and skip to next layer
        CYCLE
      ELSEIF (ABS(dv_upper).LE.epsilon) THEN	!About to form the bottom layer of a plug.
          CALL PlugTreatment(n)               !Plug treatment sets acceleration for all layers of plug above current layer, and sets them to PlugArray(n)=true, so they will be skipped in this loop.
      ELSE							!Not plug, so compute acceleration from forces
        F_1=-sign_lower*(P+rho(n)*g*d)*mu_1(n-1)
        F_2=sign_upper*P*mu_1(n)
        Acceleration(n) = (F_1+F_2)/(rho(n)*d)
      END IF
      !Now we should have an acceleration array with values correct up to at least layer n and possibly higher
    END DO

    !The following terms prevent layers from overshooting plug formation
    Do n=2, TotalLayers+1
      IF (PlugArray(n)) THEN
        CYCLE
      ELSE
        dv_lower = v(n)-v(n-1)
        dv_upper = v(n+1)-v(n)
        dv_lower_prime = dv_lower + Acceleration(n)*dt
        dv_upper_prime = dv_upper - Acceleration(n)*dt

        IF ((dv_lower*dv_lower_prime).GE.0) THEN    !No change of sign for relative velocity at lower boundary => no overshooting
          IF ((dv_upper*dv_upper_prime).GE.0) THEN
            v(n)=v(n)+Acceleration(n)*dt            !No change of sign for relative velocity at upper boundary => no overshooting
          ELSE
            v(n)=v(n+1)                             !Change of sign for relative velocity at upper boundary => set layer to have speed equal to layer above ************ IS IT POSSIBLE FOR LAYER BELOW TO APPLY SUFFICIENT FORCE TO DRAG CENTRE LAYER PAST PLUG FORMATION WITH LAYER ABOVE?  ***********************
          END IF
        ELSE
          !Change of sign for relative velocity at lower boundary
          IF ((dv_upper*dv_upper_prime).GE.0) THEN
            !If there is not also an overshoot at the top boundary, immediately set layer velocity to equal that of layer below. Otherwise set velocity equal to that of whichever layer (above or below) that they centre layer catches up to first, ie the one with smallest velocity relative to the centre layer.
            v(n)=v(n-1)
          ELSEIF ((ABS(dv_upper)).LE.(ABS(dv_lower))) THEN
            v(n)=v(n+1)
          ELSE
            v(n)=v(n-1)
          END IF
        END IF
      END IF
    END DO

    v(TotalLayers+2) = upperboundary(t)
    v(1)             = lowerboundary(t)

  end subroutine velocityupdate
end module davinci_1_velocityupdate
