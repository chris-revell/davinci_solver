!CK Revell, March 2016
!Module containing variables for DaVinci fluids solver

module davinci_0_variables

  implicit none

  !Define parameters for problem
  REAL :: mu_2 					!Static coefficient of friction
  REAL :: g							!gravitational acceleration
  REAL :: d							!Fluid layer thickness
  REAL :: epsilon     	!Accuracy to which velocity differences are defined as zero or non zero
  REAL :: rho_0					!Loose random packing density of grains, assumed

  !Define variables for problem
  REAL P_0												!Pressure exerted by upper boundary
  REAL dt													!Time interval
  REAL P													!Normal force
  REAL F_1												!Lower frictional force
  REAL F_2												!Upper frictional force
  REAL dv_lower												!Lower velocity difference
  REAL dv_upper												!Upper velocity difference
  REAL dv_lower_prime									!Lower velocity difference after acceleration of layer
  Real dv_upper_prime									!Upper velocity difference after acceleration of layer
  REAL sign_lower												!Sign of lower velocity difference
  REAL sign_upper												!Sign of upper velocity difference
  INTEGER t												!Time counter
  INTEGER n												!Useful counter
  INTEGER FunctionChoice					!For choosing between preprogrammed functions. Ranges from 1 to 6.
  INTEGER TotalLayers							!Total number of layers in fluid
  INTEGER TotalTime								!Number of dt intervals after which to end simulation
  INTEGER :: TimeOut
  CHARACTER(LEN=8) :: date        !Date of simulation run
  CHARACTER(LEN=4) :: time        !Time of simulation run
  CHARACTER(LEN=13):: output_folder !Name of folder created for data output, labelled according to date and time of run.
  INTEGER i												!Index to be summed over
  REAL MassAboveLayer							!Mass of fluid above the layer under consideration
  INTEGER FileOrFunction					!Value determined at command line - used to tell code which algorithm to run
  CHARACTER(LEN=11) InputFilename	!Filename of initial velocity profile input file
  CHARACTER(LEN=5) Timebound			!For gnuplot file - equal to (TotalTime+1)
  CHARACTER(LEN=5) Layerbound			!For gnuplot file - equal to (TotalLayers+2)
  REAL alpha											!Parameter that sets exponential variation of fluid density (see FUNCTION DensityRelation(n))
  REAL K													!Factor for density relation that should include velocity dependence but is for now constant. K=rho_0 + P_0 + f(v). For now set f(v)=0.
  REAL H													!Height of fluid. H=TotalLayers*d

end module davinci_0_variables
