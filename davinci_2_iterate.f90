!CK Revell, March 2016
!Subroutine containing module to iterate system over all timesteps

module davinci_2_iterate

  use davinci_0_arrays
  use davinci_0_variables
  use davinci_1_densityrelation
  use davinci_1_frictionrelation
  use davinci_1_plugtreatment
  use davinci_1_output
  use davinci_1_velocityupdate

  implicit none

contains

  subroutine iterate

    !Calculate velocity profile at all remaining timesteps and write to file.
    DO t=1, TotalTime+1									!Time ticker must run from 1 since this is the index of the first array element

      !Recalculate dynamic friction coefficient array
      Do n=1, TotalLayers+1
        mu_1(n) = FrictionRelation(rho(n), rho(n+1))
      END DO

!Why refresh plug array?
!**************************************************************
      !Refresh PlugArray for each new timestep
      Do n=1, TotalLayers+2
        PlugArray(n)=.FALSE.
      END DO
!**************************************************************




!Do we need this section?
!**************************************************************
      !Refresh acceleration array
!      Do n=1, TotalLayers+2
!        Acceleration(n)=0.0
!      END DO
!**************************************************************




      call velocityupdate

      !Recalculate density array
      Do n=2, TotalLayers+1
        rho(n) = DensityRelation(n)
      END DO

      !Output timestep to file 
      call output

    END DO

  end subroutine iterate
end module davinci_2_iterate
