PROGRAM SOLVER_rho3

	!Code to simulate da Vinci fluid under shear stress. Modelled as N layers of thinkness d, with a slab on top being sheared at constant velocity.
	!Dynamic friction coefficient is given by array MuProfile as it varies over the thickness of the fluid. Static friction coefficient constant and given by mu_2. mu_1<mu_2.
	!Fluid mass density is rho and the slab on top of the fluid applies a pressure M.
	!Contains subroutines and functions to calculate initial velocity and mu profiles.

	use davinci_0_arrays
	use davinci_0_variables
	use davinci_1_plugtreatment
	use davinci_1_densityrelation
	use davinci_1_frictionrelation

	IMPLICIT NONE

	!Initialise system
	call initialise

	!Output to command line the command that must now be entered into gnuplot in order to plot these results
!	WRITE(*,*) "Copy the following lines into Gnuplot to plot these results"
!	WRITE(*,*) "load 'RhoGnuplotCommandsPng"//x//".txt'"
!	WRITE(*,*) "load 'RhoGnuplotCommandsFig"//x//".txt'"

	!Write initial system to file
	call output

	call iterate

!	CLOSE(6)
!	CLOSE(7)

	!create temporary file to obtain character form of (TotalLayers+2) and (TotalTime+1)
	OPEN(5, FILE = "temp.txt")
	WRITE(5,*) (TotalLayers+2)
	WRITE(5,*) (TotalTime+1)
	CLOSE(5)
	OPEN(5, FILE = "temp.txt")
	READ(5,*) Layerbound
	READ(5,*) Timebound
	CLOSE(5)

	call gnuplot_output

	call output_final

END PROGRAM SOLVER_rho3
