!CK Revell, March 2016
!Module containining density relation for da vinci solver
!Relates layer density to height

module davinci_1_densityupdate

  use davinci_0_arrays
  use davinci_0_variables

  implicit none

contains

  subroutine DensityUpdate

    real :: P     !Pressure on a given layer.

    P = P_0

    do n=1, TotalLayers+2
      !Working from top down
      if (v(TotalLayers+3-n).LT.(1.0)) then
        rho(TotalLayers+3-n) = rho_0*P
        print*, rho_0*P
      else
        rho(TotalLayers+3-n) = rho_0*P/(v(TotalLayers+3-n)**2)    !Derived by analogy to ideal gas law. Need to check units and value of constant factor rho_0
        print*, rho_0*P/(v(TotalLayers+3-n)**2)
      endif
      P = P + rho(TotalLayers+3-n)*d*g                  !Increase pressure on the next layer by the weight of this layer
    enddo

  END subroutine DensityUpdate

end module davinci_1_densityupdate

!densityupdate = 2*rho_0*LOG(TotalLayers-0.5*n) !Pulled off the top of my head
