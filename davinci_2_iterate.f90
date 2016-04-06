!CK Revell, March 2016
!Subroutine containing module to iterate system over all timesteps

module davinci_2_iterate

  use davinci_0_arrays
  use davinci_0_variables
  use davinci_1_densityupdate
  use davinci_1_frictionupdate
  use davinci_1_plugtreatment
  use davinci_1_output
  use davinci_1_velocityupdate

  implicit none

contains

  subroutine iterate

    !Calculate velocity profile at all remaining timesteps and write to file.
    DO WHILE (t.LE.TotalTime)

      !Refresh PlugArray for each new timestep
      Do n=1, TotalLayers+2
        PlugArray(n)=.FALSE.
      END DO

      call velocityupdate

      !Recalculate density array
      call densityupdate

      !Recalculate dynamic friction array
      call frictionupdate

      !Output timestep to file
      if (mod(t,TimeOut).EQ.0) then
        call output
      endif

      t = t + dt

    END DO

  end subroutine iterate
end module davinci_2_iterate
