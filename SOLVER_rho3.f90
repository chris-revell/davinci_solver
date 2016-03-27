PROGRAM SOLVER_rho3

!Code to simulate da Vinci fluid under shear stress. Modelled as N layers of thinkness d, with a slab on top being sheared at constant velocity. 

!Dynamic friction coefficient is given by array MuProfile as it varies over the thickness of the fluid. Static friction coefficient constant and given by mu_2. mu_1<mu_2.

!Fluid mass density is rho and the slab on top of the fluid applies a pressure M. 

!Contains subroutines and functions to calculate initial velocity and mu profiles. 

IMPLICIT NONE

!Set parameters for problem
REAL, PARAMETER :: mu_2 =0.5					!Static coefficient of friction
REAL, PARAMETER :: g=10							!gravitational acceleration
REAL, PARAMETER :: d=1							!Fluid layer thickness
REAL, PARAMETER :: epsilon=0.0000001			!Accuracy to which velocity differences are defined as zero or non zero
REAL, PARAMETER :: rho_0=1						!Loose random packing density of grains, assumed 

!Define variables for problem
REAL P_0										!Pressure exerted by upper boundary
REAL dt											!Time interval
REAL P											!Normal force
REAL F_1										!Lower frictional force
REAL F_2										!Upper frictional force
REAL dv_1										!Lower velocity difference
REAL dv_2										!Upper velocity difference
REAL dv_1_prime									!Lower velocity difference after acceleration of layer
Real dv_2_prime									!Upper velocity difference after acceleration of layer
REAL s_1										!Sign of lower velocity difference
REAL s_2										!Sign of upper velocity difference
REAL v_top										!Boundary condition - upper boundary velocity
REAL v_bottom									!Boundary condition - lower boundary velocity (normally zero)
INTEGER t										!Time counter
INTEGER n										!Layer counter
INTEGER FunctionChoice							!For choosing between preprogrammed functions. Ranges from 1 to 6. 
INTEGER TotalLayers								!Total number of layers in fluid
INTEGER TotalTime								!Number of dt intervals after which to end simulation
CHARACTER(LEN=2) x								!Numerical label for friction and initial velocity profiles
INTEGER i										!Index to be summed over
REAL MassAboveLayer								!Mass of fluid above the layer under consideration
INTEGER FileOrFunction							!Value determined at command line - used to tell code which algorithm to run
CHARACTER(LEN=11) InputFilename					!Filename of initial velocity profile input file
CHARACTER(LEN=5) Timebound						!For gnuplot file - equal to (TotalTime+1)
CHARACTER(LEN=5) Layerbound						!For gnuplot file - equal to (TotalLayers+2)
REAL alpha										!Parameter that sets exponential variation of fluid density (see FUNCTION DensityRelation(n))
REAL K											!Factor for density relation that should include velocity dependence but is for now constant. K=rho_0 + P_0 + f(v). For now set f(v)=0.
REAL H											!Height of fluid. H=TotalLayers*d

REAL, DIMENSION(:), allocatable :: mu_1			!Define array for variation in dynamic friction coefficient

REAL, DIMENSION(:), allocatable :: v			!Velocity array having an element for each layer of fluid at each position in time. Velocities for v(1) and v(TotalLayers+2) will not change. These are the boundaries confining the fluid. Array will be outputted as data. 

REAL, DIMENSION(:), allocatable :: Acceleration	!Acceleration array

LOGICAL, DIMENSION(:), allocatable :: PlugArray	!Has value 1 for all layers that are part of a plug and 0 for all other layers

REAL, DIMENSION(:), allocatable :: rho			!Density array


PRINT*, "Enter results label x (must be two digit number eg. 01 not 1)"
READ*, x

PRINT*, "Enter time interval dt"
READ*, dt

PRINT*, "Enter upper boundary pressure P_0"
READ*, P_0

PRINT*, "Enter value for alpha (alpha>0), to set the precise form of the exponential decay of density with height"
PRINT*, "(velocity dependence ignored for now)"
READ*, alpha

!Now that alpha and P_0 have been evaluated, we can evaluate K for use in the density variation function. 
K=rho_0+alpha*P_0

!Choose whether the initial velocity profile will be read from a file or defined by a function. 
PRINT*, "Initial Velocity Profile from file (enter 1) or function (enter 2)?"
READ*, FileOrFunction


IF (FileOrFunction.EQ.1) THEN !Set system up and read initial profile from a file
	PRINT*, "Enter input filename (don't include extension, should be .txt); filename" 
	PRINT*, " without extension must be 11 characters long). Note file should"
	PRINT*, "include a value of TotalLayers as the first data point and end with a blank line."		
	READ*, InputFilename
	Print*, "Enter total number of time intervals"
	READ*, TotalTime
	PRINT*, "Specify boundary conditions."
	PRINT*, "Enter upper boundary velocity:"
	READ*, v_top
	PRINT*, "Enter lower boundary velocity:"
	PRINT*, "(it is anticipated that this will normally be zero but the ability to change it is included in the code)"
	READ*, v_bottom
	
	!Initialise velocity array from file:
	OPEN(2, FILE = ''//InputFilename//'.txt')
	
	READ(2, *) TotalLayers
	ALLOCATE(v(TotalLayers+2))
	Do n=2, TotalLayers+1
		READ(2, *) v(n)
	END DO
	v(1) = v_bottom
	v(TotalLayers+2) = v_top
	
	!Allocate array dimensions
	ALLOCATE(mu_1(TotalLayers+1))
	ALLOCATE(Acceleration(TotalLayers+2))
	ALLOCATE(PlugArray(TotalLayers+2))
	ALLOCATE(rho(TotalLayers+2))
	
ELSE !set up system, reating initial profile from a function
	PRINT*, "Choose preprogrammed function"
	PRINT*, "Enter 1 for whole fluid at rest"
	PRINT*, "Enter 2 for a linear velocity profile"
	PRINT*, "Enter 3 for a parabolic profile with maximum at the centre"
	PRINT*, "Enter 4 for a parabolic profile with maxima at the boundaries"
	PRINT*, "Enter 5 for two plugs: one at each boundary, the upper one at the same velocity as the upper boundary, the other at rest"
	PRINT*, "Enter 6 for two plugs: one at each boundary, the upper one at a lower velocity than the upper boundary, the other at rest"
	PRINT*, "Do not enter a character other than integers 1-6"
	READ*, FunctionChoice
	Print*, "Enter total number of fluid layers (excluding container)"
	READ*, TotalLayers
	Print*, "Enter total number of time intervals"
	READ*, TotalTime
	
	!Allocate array dimensions
	ALLOCATE(mu_1(TotalLayers+1))
	ALLOCATE(v(TotalLayers+2))
	ALLOCATE(Acceleration(TotalLayers+2))
	ALLOCATE(PlugArray(TotalLayers+2))
	ALLOCATE(rho(TotalLayers+2))
	
	PRINT*, "Specify boundary conditions."
	PRINT*, "Enter upper boundary velocity:"
	READ*, v_top
	PRINT*, "Enter lower boundary velocity:"
	PRINT*, "(it is anticipated that this will normally be zero but the ability to change it is included in the code)"
	READ*, v_bottom
	
	!Initialise velocity array
	v(1) = v_bottom
	v(TotalLayers+2) = v_top
	Do n=2, TotalLayers+1
		v(n) = InitialVelocityFunction(n, FunctionChoice)
	END DO
		
END IF

!Now that TotalLayers has been evaluated, evaluate H
H=TotalLayers*d

!Output to command line the command that must now be entered into gnuplot in order to plot these results
PRINT*, "Copy the following lines into Gnuplot to plot these results"
PRINT*, "load 'RhoGnuplotCommandsPng"//x//".txt'"
PRINT*, "load 'RhoGnuplotCommandsFig"//x//".txt'"

!Initialise density array
Do n=1, TotalLayers+2
	rho(n) = DensityRelation(n)
END DO	

!Initialise dynamic friction array
Do n=1, TotalLayers+1
	mu_1(n) = MuRelation(rho(n), rho(n+1))
END DO	

!Initialise Acceleration array
Do n=1, TotalLayers+2
	Acceleration(n)=0.0
END DO

!Initialise PlugArray
Do n=1, TotalLayers+2
	PlugArray(n)=.FALSE.
END DO

!Open file for results
OPEN(6,FILE = 'RhoResults'//x//'.txt') 

!Write initial profile to results file
WRITE (6,*) (v(n), n=1,TotalLayers+2)

!Calculate velocity profile at all remaining timesteps and write to file. 
DO t=2, TotalTime+1									!Time ticker must run from 1 since this is the index of the first array element

	!Refresh PlugArray for each new timestep
	Do n=1, TotalLayers+2
		PlugArray(n)=.FALSE.
	END DO
	
	!Refresh acceleration array
	Do n=1, TotalLayers+2
		Acceleration(n)=0.0
	END DO
	
	
	DO n=2, TotalLayers+1		!Note starts from 2 and ends at TotalLayers+1 because the n=1 and n=TotalLayers+2 layers are boundaries whose velocities are controlled externally and do not change. 
	
		!Compute velocity differences
		dv_1=v(n)-v(n-1)
		dv_2=v(n+1)-v(n)
		
		!Compute signs of upper and lower velocity differences
		s_1=SIGN(1.0,dv_1)
		s_2=SIGN(1.0,dv_2)
		
			
		!Compute normal force on upper edge of layer n
		MassAboveLayer=0
		Do i=(n+1), TotalLayers+1
			MassAboveLayer = MassAboveLayer + rho(i)*d
		END DO
		P=P_0+MassAboveLayer*g
		
		IF (PlugArray(n)) THEN
			!Do nothing and skip to next layer
			go to 70
		ELSE
			IF (ABS(dv_2).LE.epsilon) THEN	!Plug, so use plug treatment to find acceleration
				CALL PlugTreatment(n, TotalLayers, v, Acceleration, PlugArray, mu_1)	
			ELSE							!Not plug, so compute acceleration from forces
				F_1=-s_1*(P+rho(n)*g*d)*mu_1(n-1)
				F_2=s_2*P*mu_1(n)
				Acceleration(n) = (F_1+F_2)/(rho(n)*d)
			END IF
		END IF
		!Now we should have an acceleration array with values correct up to at least layer n and possibly higher
		70	continue	
	END DO
	
	!The following terms prevent layers from overshooting plug formation
	Do n=2, TotalLayers+1
		IF (PlugArray(n)) THEN 
			GO TO 17
		ELSE
			dv_1 = v(n)-v(n-1)
			dv_2 = v(n+1)-v(n)
			dv_1_prime = dv_1 + Acceleration(n)*dt
			dv_2_prime = dv_2 - Acceleration(n)*dt
			
			IF ((dv_1*dv_1_prime).GE.0) THEN
				GO TO 1
			ELSE
				GO TO 2
			END IF
			
			1 CONTINUE
			IF ((dv_2*dv_2_prime).GE.0) THEN
				v(n)=v(n)+Acceleration(n)*dt
			ELSE
				v(n)=v(n+1)
			END IF
			GO TO 3
			
			2 CONTINUE
			IF ((dv_2*dv_2_prime).GE.0) THEN
				v(n)=v(n-1)
				GO TO 3
			ELSE
				IF ((ABS(dv_2)).LE.(ABS(dv_1))) THEN
					v(n)=v(n+1)
				ELSE
					v(n)=v(n-1)
				END IF
			END IF
		
			3 CONTINUE
		END IF
		17 CONTINUE


	END DO
	
	!Recalculate density array
	Do n=2, TotalLayers+1
		rho(n) = DensityRelation(n)
	END DO
	
	!Recalculate dynamic friction coefficient array
	Do n=1, TotalLayers+1
		mu_1(n) = MuRelation(rho(n), rho(n+1))
	END DO	

	!Output data to file
	WRITE (6,*) (v(n), n=1,TotalLayers+2)	
	
END DO

CLOSE(6)

!create temporary file to obtain character form of (TotalLayers+2) and (TotalTime+1)
OPEN(5, FILE = "temp.txt")
WRITE(5,*) (TotalLayers+2)
WRITE(5,*) (TotalTime+1)
CLOSE(5)
OPEN(5, FILE = "temp.txt")
READ(5,*) Layerbound
READ(5,*) Timebound
CLOSE(5)

!Create gnuplot command files for both fig and png outputs
OPEN(3, FILE = "RhoGnuplotCommandsPng"//x//'.txt')
!WRITE (3,*) 'set terminal png'
!WRITE (3,*) 'set output "RhoPlot'//x//'.png"'
WRITE (3,*) 'set xlabel "Fluid height (n)"'
WRITE (3,*) 'set xrange [ 0 : '//Layerbound//' ] noreverse nowriteback'
WRITE (3,*) 'set xlabel  offset character 3, -2, 0 font "" textcolor lt -1 rotate by 90'
WRITE (3,*) 'set ylabel "Time"'
WRITE (3,*) 'set ylabel  offset character -3, -2, 0 font "" textcolor lt -1 norotate'
WRITE (3,*) 'set yrange [ 0 : '//Timebound//' ] noreverse nowriteback'
WRITE (3,*) 'set zlabel "Velocity"'
WRITE (3,*) 'set zlabel  offset character -2, 0, 0 font "" textcolor lt -1 norotate'
WRITE (3,*) 'splot "RhoResults'//x//'.txt" matrix with pm3d  t  "Evolution of velocity profile with time"'
CLOSE(3)
!OPEN(2, FILE = "RhoGnuplotCommandsFig"//x//'.txt')
!WRITE (2,*) 'set terminal fig color'
!WRITE (2,*) 'set output "RhoPlot'//x//'.fig"'
!WRITE (2,*) 'set xlabel "Fluid height (n)"'
!WRITE (2,*) 'set xrange [ 0 : '//Layerbound//' ] noreverse nowriteback'
!WRITE (2,*) 'set xlabel  offset character 3, -2, 0 font "" textcolor lt -1 rotate by 90'
!WRITE (2,*) 'set ylabel "Time"'
!WRITE (2,*) 'set ylabel  offset character -3, -2, 0 font "" textcolor lt -1 norotate'
!WRITE (2,*) 'set yrange [ 0 : '//Timebound//' ] noreverse nowriteback'
!WRITE (2,*) 'set zlabel "Velocity"'
!WRITE (2,*) 'set zlabel  offset character -2, 0, 0 font "" textcolor lt -1 norotate'
!WRITE (2,*) 'splot "RhoResults'//x//'.txt" matrix with pm3d  t  "Evolution of velocity profile with time"'
!CLOSE(2)

!Create file to store results parameters
OPEN(4, FILE = "RhoParameters"//x//".txt")
WRITE(4,*) "Results label: "//x
IF (FileOrFunction.EQ.1) THEN
	WRITE(4,*) "Input filename: "//InputFilename//".txt"
ELSE
	WRITE(4,*) "Initial velocity profile function choice:"
	WRITE(4,*) FunctionChoice
END IF
WRITE(4,*) "Total time:"
WRITE(4,*) TotalTime
WRITE(4,*) "Total number of fluid layers:"
WRITE(4,*) TotalLayers
WRITE(4,*) "dt:"
WRITE(4,*) dt
WRITE(4,*) "P_0:"
WRITE(4,*) P_0
WRITE(4,*) "Static friction coefficient"
WRITE(4,*) mu_2
WRITE(4,*) "Accuracy, epsilon:"
WRITE(4,*) epsilon
WRITE(4,*) "Layer thickness, d:"
WRITE(4,*) d
WRITE(4,*) "Alpha:"
WRITE(4,*) alpha
CLOSE(4)


CONTAINS
	
	SUBROUTINE PlugTreatment(n, TotalLayers, v, Acceleration, PlugArray, mu_1)
		
		INTEGER TotalLayers
		INTEGER n
		REAL, DIMENSION(TotalLayers+2), INTENT(INOUT) :: v
		REAL, DIMENSION(TotalLayers+2), INTENT(INOUT) :: Acceleration
		LOGICAL, DIMENSION(TotalLayers+2), INTENT(INOUT):: PlugArray
		REAL, DIMENSION(TotalLayers+1), INTENT(IN) :: mu_1
			
		INTEGER i
		INTEGER :: a=0					!Plug layer counter. Number of layers in plug. 
		INTEGER :: b=0					!Plug counter after split
		REAL :: PlugCondition = 0.0		!Effectively dv_2. Value allows us to determine the end of a plug.
		REAL PlugAcceleration
		REAL W							!Pressure applied to top surface of plug
		INTEGER s_bottom				!Redefine s_1 inside subroutine for the bottom layer of the plug 
		INTEGER s_top					!Redefine s_2 inside subroutine for the top layer of the plug 
		REAL F_bottom					!Force on bottom of plug
		REAL F_top						!Force on top of plug
		REAL PlugMass					!PlugMass is the mass of the plug
		REAL MassAbovePlug				!MassAbovePlug is the mass of fluid above the plug
		REAL RequiredForce				!Force that must be applied to the top of a layer in the plug to maintain the same acceleration for this layer as for the rest of the plug
		REAL FrictionalForce			!Maximum force that can be applied at the top of the layer within the plug
		REAL F_F_1						!Frictional force expression too long, so expression broken into parts
		REAL R_F_1						!Same for required force

		a=0
		b=0
		PlugCondition=0.0

		!Calculate number of layers in plug
  81	IF ( a .LE. (TotalLayers-n+1)) then
			IF (PlugCondition.LT.epsilon) then
				PlugCondition = ABS(v(n+a+1)-v(n+a))
				a=a+1
				go to 81
			ELSE
			go to 82
			ENDIF
		ELSE
		   go to 82
		ENDIF
 82		continue
			
			
		
		!Set W and MassAbovePlug
		MassAbovePlug = 0
		DO i=n+a, TotalLayers+1
			MassAbovePlug = MassAbovePlug + rho(i)*d
		END DO
		W = MassAbovePlug*g + P_0			
		
		!Set PlugMass
		PlugMass = 0
		DO i=n, (n+a-1)
			PlugMass = PlugMass +rho(i)*d
		END DO									
		
		
		s_bottom=SIGN(1.0,(v(n)-v(n-1)))						!Signs of velocity differences at the top and bottom of the plug
		s_top=SIGN(1.0,(v(n+a)-v(n+a-1)))
	
		IF (((n+a-1).EQ.TotalLayers+1).AND.(ABS(v(n+a)-v(n+a-1)).LT.epsilon)) THEN	!Plug stationary relative to upper boundary
			F_bottom=-REAL(s_bottom)*(W+PlugMass*g)*mu_1(n-1)
			F_top=-F_bottom										
		ELSE
			F_top=REAL(s_top)*W*mu_1(n+a-1)
		END IF
	
		IF ((n.EQ.2).AND.(ABS(v(n)-v(n-1)).LT.epsilon)) THEN 		!Condition for plug stationary relative to lower boundary
			IF (ABS(F_top).GT.ABS((W+PlugMass*g)*mu_2)) THEN
				F_bottom=-REAL(s_top)*(W+PlugMass*g)*mu_2
			ELSE
				F_bottom=-F_top			!This is the max value F_bottom can take in this case, as required for splitting calculations. It is not necessarily the force to use for calculating the plug acceleration.
			END IF
		ELSE
			F_bottom=-REAL(s_bottom)*(W+PlugMass*g)*mu_1(n-1)	!Force on bottom of plug for general plug moving relative to lower boundary or not touching lower boundary
		END IF
		
		PlugAcceleration = (F_top + F_bottom)/PlugMass
		
		
		!Also need to consider behaviour if both boundaries are in contact with plug and stationary relative to it
		IF ((((n+a-1).EQ.TotalLayers+1).AND.(ABS(v(n+a)-v(n+a-1)).LT.epsilon)).AND.((n.EQ.1).AND.(ABS(v(n)-v(n-1)).LT.epsilon))) THEN
			PlugAcceleration=0	
		END IF		
		
		
		DO i=0, (a-1)
		
			F_F_1 = (EXP(-alpha*g*d*(n+i))-EXP(-alpha*g*d*(TotalLayers+1)))/(1-EXP(-alpha*g*d))			
			FrictionalForce = mu_2*P_0+mu_2*d*g*K*EXP(alpha*g*d*TotalLayers)*F_F_1								 !Frictional force that actually is applied on the top of layer 
			
			R_F_1 = (EXP(-alpha*g*d*n)-EXP(-alpha*g*d*(n+i+1)))/(1-EXP(-alpha*g*d))
			RequiredForce = d*PlugAcceleration*K*EXP(alpha*g*d*(TotalLayers+1))*R_F_1 - F_bottom 	!Stress that must be applied to the top of the layer under consideration in order to maintain the same acceleration as the rest of the plug give some stress on the bottom
				
			IF (ABS(FrictionalForce).LT.ABS(RequiredForce)) THEN		!If max frictional force is smaller than that which would be required to maintain the same acceleration in this layer as the rest of the plug
				PlugArray(n+i) = .TRUE.									!This layer is still part of the lower plug
				b=b+1													!Lower plug layer counter incremented
				GO TO 83												!Exit DO statement and do not consider the remaining layers in the original plug or set their PlugArray values to zero.
			ELSE
				PlugArray(n+i) = .TRUE.
				b=b+1
			END IF
		END DO
		
		!Redefine signs and forces for the new bottom plug. If the plug does not split, a=b so these will work out the same anyway. 		
		83 CONTINUE
		MassAbovePlug = 0
		DO i=n+b, TotalLayers+1
			MassAbovePlug = MassAbovePlug + rho(i)*d
		END DO
		W = MassAbovePlug*g + P_0			
		
		!Set new PlugMass
		PlugMass = 0
		DO i=n, (n+b-1)
			PlugMass = PlugMass +rho(i)*d
		END DO	
		
		s_bottom=SIGN(1.0,(v(n)-v(n-1)))
		s_top=SIGN(1.0,(v(n+b)-v(n+b-1)))
	
		F_top=REAL(s_bottom)*W*mu_1(n+b-1)		!No need to redefine F_bottom
		
		PlugAcceleration = (F_top + F_bottom)/PlugMass
		
		
		!The following terms prevent layers from overshooting plug formation

		dv_1 = v(n)-v(n-1)
		dv_2 = v(n+b)-v(n+b-1)
		dv_1_prime = dv_1 + PlugAcceleration*dt
		dv_2_prime = dv_2 - PlugAcceleration*dt
		
		IF ((dv_1*dv_1_prime).GE.0) THEN
			GO TO 1
		ELSE
			GO TO 2
		END IF
		
		1 CONTINUE
		IF ((dv_2*dv_2_prime).GE.0) THEN
			DO i=0, b-1
				v(n+i)=v(n+i)+PlugAcceleration*dt
			END DO
		ELSE
			DO i=0, b-1
				v(n+i)=v(n+b)
			END DO
		END IF
		GO TO 3
		
		2 CONTINUE
		IF ((dv_2*dv_2_prime).GE.0) THEN
			DO i=0, b-1
				v(n+i)=v(n-1)
			END DO
			GO TO 3
		ELSE
			IF ((ABS(dv_2)).LE.(ABS(dv_1))) THEN		!Forms plug with the first one it catches up to
				DO i=0, b-1
					v(n+i)=v(n+b)
				END DO
			ELSE
				DO i=0, b-1
					v(n+i)=v(n-1)
				END DO
			END IF
		END IF
		
		3 CONTINUE
		
			
	END SUBROUTINE PlugTreatment
			
		
	REAL FUNCTION InitialVelocityFunction(n, FunctionChoice) 					!Function describing velocity profile.
		
		INTEGER n
		INTEGER FunctionChoice
		
		IF (FunctionChoice.EQ.1) THEN !At rest
			InitialVelocityFunction = 0
			
		ELSE IF (FunctionChoice.EQ.2) THEN	!Linear
			InitialVelocityFunction = v_bottom + (n-1)*(v_top-v_bottom)/(TotalLayers+1)
			
		ELSE IF (FunctionChoice.EQ.3) THEN	!Parabolic with maximum in middle
			InitialVelocityFunction = (v_bottom + (TotalLayers+1)*(TotalLayers+1)/4 - ((n-1)-(TotalLayers+1)/2)*((n-1)-(TotalLayers+1)/2))
		
		ELSE IF (FunctionChoice.EQ.4) THEN !Parabolic with maxima at edges
			InitialVelocityFunction = v_bottom + ((n-1)-(TotalLayers+1)/2)*((n-1)-(TotalLayers+1)/2)
		
		ELSE IF (FunctionChoice.EQ.5) THEN !2 plugs, upper at speed of upper boundary
			IF (n.LT.((TotalLayers+2)/3)) THEN
				InitialVelocityFunction = v_bottom
			ELSE IF (n.GT.(2*(TotalLayers+2)/3)) THEN
				InitialVelocityFunction = v_top
			ELSE
				InitialVelocityFunction = v_bottom + (n-NINT(REAL((TotalLayers+2)/3)))*(3*(v_top-v_bottom)/(TotalLayers+2))
			END IF
			
		ELSE IF (FunctionChoice.EQ.6) THEN	!2 plugs, upper below speed of upper boundary
			IF (n.LT.((TotalLayers+2)/3)) THEN
				InitialVelocityFunction = v_bottom
			ELSE IF (n.GT.(2*(TotalLayers+2)/3)) THEN
				InitialVelocityFunction = 0.75*v_top
			ELSE
				InitialVelocityFunction = v_bottom + (n-NINT(REAL((TotalLayers+2)/3)))*(3*(0.75*v_top-v_bottom)/(TotalLayers+2))
			END IF
			
		END IF

	END FUNCTION
		
	
	REAL FUNCTION DensityRelation(n)
		INTEGER n
		REAL z
		z=REAL(n-1)*d
		DensityRelation = K*EXP(alpha*g*(H-z))
	END FUNCTION
		
		
	REAL FUNCTION MuRelation(rho_1, rho_2)
		REAL rho_1
		REAL rho_2
		MuRelation = mu_2*EXP(2*alpha*g*d*(1-TotalLayers))*rho_1*rho_2/(K*K)
	END FUNCTION
	
END PROGRAM SOLVER_rho3