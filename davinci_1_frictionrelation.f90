!CK Revell, March 2016
!Module containing the friction relation for the da vinci fluids solver

module davinci_1_frictionrelation

  use davinci_0_arrays
  use davinci_0_variables

  implicit none

contains

  real FUNCTION FrictionRelation(rho_1, rho_2)
    REAL, intent(in) :: rho_1
    REAL, intent(in) :: rho_2
    FrictionRelation = mu_2*EXP(2*alpha*g*d*(1-TotalLayers))*rho_1*rho_2/(K*K)
  END FUNCTION FrictionRelation

end module davinci_1_frictionrelation
