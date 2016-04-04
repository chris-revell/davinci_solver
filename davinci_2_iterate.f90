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

      !Refresh PlugArray for each new timestep
      Do n=1, TotalLayers+2
        PlugArray(n)=.FALSE.
      END DO

!Do we need this section?
!**************************************************************
      !Refresh acceleration array
!      Do n=1, TotalLayers+2
!        Acceleration(n)=0.0
!      END DO
!**************************************************************

      call velocityupdate
      write(*,*) Acceleration(:)
      !Recalculate density array
      Do n=1, TotalLayers+2
        rho(n) = DensityRelation(n)
      END DO

      !Recalculate dynamic friction array
      Do n=2, TotalLayers+2
        mu_1(n) = FrictionRelation(n)
      END DO

      !Output timestep to file
      if (mod(t,TimeOut).EQ.0) then
        call output
      endif

    END DO

  end subroutine iterate
end module davinci_2_iterate
