!CK Revell, March 2016
!Module containing variables for DaVinci fluids solver

module davinci_0_variables

  implicit none

  !Set parameters for problem
  REAL, PARAMETER :: mu_2 =0.5					!Static coefficient of friction
  REAL, PARAMETER :: g=10								!gravitational acceleration
  REAL, PARAMETER :: d=1								!Fluid layer thickness
  REAL, PARAMETER :: epsilon=0.0000001	!Accuracy to which velocity differences are defined as zero or non zero
  REAL, PARAMETER :: rho_0=1						!Loose random packing density of grains, assumed

  !Define variables for problem
  REAL P_0												!Pressure exerted by upper boundary
  REAL dt													!Time interval
  REAL P													!Normal force
  REAL F_1												!Lower frictional force
  REAL F_2												!Upper frictional force
  REAL dv_1												!Lower velocity difference
  REAL dv_2												!Upper velocity difference
  REAL dv_1_prime									!Lower velocity difference after acceleration of layer
  Real dv_2_prime									!Upper velocity difference after acceleration of layer
  REAL s_1												!Sign of lower velocity difference
  REAL s_2												!Sign of upper velocity difference
!  REAL v_top											  !Boundary condition - upper boundary velocity
!  REAL v_bottom									  !Boundary condition - lower boundary velocity (normally zero)
  INTEGER t												!Time counter
  INTEGER n												!Layer counter
  INTEGER FunctionChoice					!For choosing between preprogrammed functions. Ranges from 1 to 6.
  INTEGER TotalLayers							!Total number of layers in fluid
  INTEGER TotalTime								!Number of dt intervals after which to end simulation
!  CHARACTER(LEN=2) x						   	!Numerical label for friction and initial velocity profiles
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
