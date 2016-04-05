!CK Revell, March 2016
!Module containing the friction relation for the da vinci fluids solver
!Relates dynamic friction at a boundary to the density on either side of those boundaries

module davinci_1_frictionrelation

  use davinci_0_arrays
  use davinci_0_variables

  implicit none

contains

  real FUNCTION FrictionRelation(n)
    INTEGER, intent(in) :: n
    REAL :: rho_1
    REAL :: rho_2

    rho_1 = rho(n)
    rho_2 = rho(n+1)

    FrictionRelation = mu_2*(rho_1+rho_2)/(rho_1*rho_2) !This formula pulled off the top of my head as the first thing I could think of that would ensure mu_1<mu_2 and combined both rho_1 and rho_2             !mu_2*EXP(2*alpha*g*d*(1-TotalLayers))*rho_1*rho_2/(K*K)

  END FUNCTION FrictionRelation

end module davinci_1_frictionrelation
