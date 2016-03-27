PROGRAM SOLVER_rho3

	!Code to simulate da Vinci fluid under shear stress. Modelled as N layers of thinkness d, with a slab on top being sheared at constant velocity.
	!Dynamic friction coefficient is given by array MuProfile as it varies over the thickness of the fluid. Static friction coefficient constant and given by mu_2. mu_1<mu_2.
	!Fluid mass density is rho and the slab on top of the fluid applies a pressure M.
	!Contains subroutines and functions to calculate initial velocity and mu profiles.

	use davinci_1_gnuplot
	use davinci_1_output_final
	use davinci_1_output
	use davinci_2_initialise
	use davinci_2_iterate 

	IMPLICIT NONE

	!Initialise system
	call initialise

	!Write initial system to file
	call output

	!Iterate system over all timesteps
	call iterate

	!Create gnuplot command file for plotting data
!	call gnuplot_output

	!Output system parameters to file
	call output_final

END PROGRAM SOLVER_rho3
