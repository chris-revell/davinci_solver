!CK Revell, March 2016
!Module containing subroutine to initialise the system for the da vinci fluids solver

module davinci_2_initialise

  use davinci_0_arrays
  use davinci_0_variables
  use davinci_1_densityrelation
  use davinci_1_frictionrelation
  use davinci_1_initialvelocityfunction
  use davinci_1_lowerboundary
  use davinci_1_upperboundary

  implicit none

contains

  subroutine initialise

    !Extract date and time, create folder to store data in
    call date_and_time(DATE=date,TIME=time)
    output_folder = date//"_"//time
    call system("mkdir data/"//output_folder)

    !Initialise parameters
    dt          = 1
    P_0         = 1
    alpha       = 1
    TotalTime   = 1000
    TotalLayers = 1000
  	K           = rho_0+alpha*P_0    !Now that alpha and P_0 have been evaluated, we can evaluate K for use in the density variation function.
    H           = TotalLayers*d      !Now that TotalLayers has been evaluated, evaluate H

    FunctionChoice = 1
    !1 for whole fluid at rest
    !2 for a linear velocity profile
    !3 for a parabolic profile with maximum at the centre
    !4 for a parabolic profile with maxima at the boundaries
    !5 for two plugs: one at each boundary, the upper one at the same velocity as the upper boundary, the other at rest
    !6 for two plugs: one at each boundary, the upper one at a lower velocity than the upper boundary, the other at rest


    !Allocate system array dimensions
    ALLOCATE(mu_1(TotalLayers+1))
    ALLOCATE(v(TotalLayers+2))
    ALLOCATE(Acceleration(TotalLayers+2))
    ALLOCATE(PlugArray(TotalLayers+2))
    ALLOCATE(rho(TotalLayers+2))

		!Initialise velocity array
		v(1)             = lowerboundary(0) !Lower boundary velocity controlled by lowerboundary(t) function
		v(TotalLayers+2) = upperboundary(0) !Upper boundary velocity controlled by upperboundary(t) function
		Do n=2, TotalLayers+1
			v(n) = InitialVelocityFunction(n, FunctionChoice)  !Initial fluid velocity set by function.
		END DO

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



!*******************************************************************************************
!This commented section is for reading initial system from file. Can dispense with it for now.

!  	READ(*,*) FileOrFunction
!  	IF (FileOrFunction.EQ.1) THEN !Set system up and read initial profile from a file
!  		READ(*,*) InputFilename
!  		READ(*,*) TotalTime
!  		READ(*,*) v_top
!  		READ(*,*) v_bottom

  		!Initialise velocity array from file:
!  		OPEN(2, FILE = ''//InputFilename//'.txt')

!  		READ(2, *) TotalLayers
!  		ALLOCATE(v(TotalLayers+2))
!  		Do n=2, TotalLayers+1
!  			READ(2, *) v(n)
!  		END DO
!  		v(1) = v_bottom
!  		v(TotalLayers+2) = v_top

  		!Allocate array dimensions
!  		ALLOCATE(mu_1(TotalLayers+1))
!  		ALLOCATE(Acceleration(TotalLayers+2))
!  		ALLOCATE(PlugArray(TotalLayers+2))
!  		ALLOCATE(rho(TotalLayers+2))

!  	ELSE !set up system, reating initial profile from a function

!*******************************************************************************************
