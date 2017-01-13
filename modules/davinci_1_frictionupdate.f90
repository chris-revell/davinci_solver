!CK Revell, March 2016
!Module containing the friction relation for the da vinci fluids solver
!Relates dynamic friction at a boundary to the density on either side of those boundaries

module davinci_1_frictionupdate

  use davinci_0_arrays
  use davinci_0_variables

  implicit none

contains

  subroutine frictionupdate

    REAL :: rho_1
    REAL :: rho_2

    do n=1, TotalLayers+1
      rho_1 = rho(n)
      rho_2 = rho(n+1)

      mu_1(n) = (mu_2/10)*(rho_1+rho_2)/2.0

    enddo



  END subroutine frictionupdate

end module davinci_1_frictionupdate
