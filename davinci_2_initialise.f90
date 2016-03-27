!CK Revell, March 2016
!Module containing subroutine to initialise the system for the da vinci fluids solver

module davinci_2_initialise

  use davinci_0_arrays
  use davinci_0_variables
  use davinci_1_densityrelation
  use davinci_1_frictionrelation
  use davinci_1_initialvelocityfunction

  implicit none

contains

  subroutine initialise

    WRITE(*,*) "Enter results label x (must be two digit number eg. 01 not 1)"
  	READ(*,*) x

  	WRITE(*,*) "Enter time interval dt"
  	READ(*,*) dt

  	WRITE(*,*) "Enter upper boundary pressure P_0"
  	READ(*,*) P_0

  	WRITE(*,*) "Enter value for alpha (alpha>0), to set the precise form of the &
  		exponential decay of density with height"
  	WRITE(*,*) "(velocity dependence ignored for now)"
  	READ(*,*) alpha

  	!Now that alpha and P_0 have been evaluated, we can evaluate K for use in the density variation function.
  	K=rho_0+alpha*P_0

  	!Choose whether the initial velocity profile will be read from a file or defined by a function.
  	WRITE(*,*) "Initial Velocity Profile from file (enter 1) or function (enter 2)?"
  	READ(*,*) FileOrFunction


  	IF (FileOrFunction.EQ.1) THEN !Set system up and read initial profile from a file
  		WRITE(*,*) "Enter input filename (don't include extension, should be .txt); filename"
  		WRITE(*,*) " without extension must be 11 characters long). Note file should"
  		WRITE(*,*) "include a value of TotalLayers as the first data point and end with a blank line."
  		READ(*,*) InputFilename
  		WRITE(*,*) "Enter total number of time intervals"
  		READ(*,*) TotalTime
  		WRITE(*,*) "Specify boundary conditions."
  		WRITE(*,*) "Enter upper boundary velocity:"
  		READ(*,*) v_top
  		WRITE(*,*) "Enter lower boundary velocity:"
  		WRITE(*,*) "(it is anticipated that this will normally be zero but the ability to change it is included in the code)"
  		READ(*,*) v_bottom

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
  		WRITE(*,*) "Choose preprogrammed function"
  		WRITE(*,*) "Enter 1 for whole fluid at rest"
  		WRITE(*,*) "Enter 2 for a linear velocity profile"
  		WRITE(*,*) "Enter 3 for a parabolic profile with maximum at the centre"
  		WRITE(*,*) "Enter 4 for a parabolic profile with maxima at the boundaries"
  		WRITE(*,*) "Enter 5 for two plugs: one at each boundary, the upper one at the &
  			same velocity as the upper boundary, the other at rest"
  		WRITE(*,*) "Enter 6 for two plugs: one at each boundary, the upper one at a &
  			lower velocity than the upper boundary, the other at rest"
  		WRITE(*,*) "Do not enter a character other than integers 1-6"
  		READ(*,*) FunctionChoice
  		WRITE(*,*) "Enter total number of fluid layers (excluding container)"
  		READ(*,*) TotalLayers
  		WRITE(*,*) "Enter total number of time intervals"
  		READ(*,*) TotalTime

  		!Allocate array dimensions
  		ALLOCATE(mu_1(TotalLayers+1))
  		ALLOCATE(v(TotalLayers+2))
  		ALLOCATE(Acceleration(TotalLayers+2))
  		ALLOCATE(PlugArray(TotalLayers+2))
  		ALLOCATE(rho(TotalLayers+2))

  		WRITE(*,*) "Specify boundary conditions."
  		WRITE(*,*) "Enter upper boundary velocity:"
  		READ(*,*) v_top
  		WRITE(*,*) "Enter lower boundary velocity:"
  		WRITE(*,*) "(it is anticipated that this will normally be zero but the &
  			ability to change it is included in the code)"
  		READ(*,*) v_bottom

  		!Initialise velocity array
  		v(1) = v_bottom
  		v(TotalLayers+2) = v_top
  		Do n=2, TotalLayers+1
  			v(n) = InitialVelocityFunction(n, FunctionChoice)
  		END DO

  	END IF

  	!Now that TotalLayers has been evaluated, evaluate H
  	H=TotalLayers*d

    !Initialise density array
    Do n=1, TotalLayers+2
      rho(n) = DensityRelation(n)
    END DO

    !Initialise dynamic friction array
    Do n=1, TotalLayers+1
      mu_1(n) = FrictionRelation(rho(n), rho(n+1))
    END DO

    !Initialise Acceleration array
    Do n=1, TotalLayers+2
      Acceleration(n)=0.0
    END DO

    !Initialise PlugArray
    Do n=1, TotalLayers+2
      PlugArray(n)=.FALSE.
    END DO

  end subroutine initialise

end module davinci_2_initialise
