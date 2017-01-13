!CK Revell, March 2016
!davinci_0_arrays.f90 contains system arrays for da vinci solver

module davinci_0_arrays

  implicit none

  REAL, DIMENSION(:), allocatable :: mu_1					!Define array for variation in dynamic friction coefficient
  REAL, DIMENSION(:), allocatable :: v						!Velocity array having an element for each layer of fluid at each position in time. Velocities for v(1) and v(TotalLayers+2) will not change. These are the boundaries confining the fluid. Array will be outputted as data.
  REAL, DIMENSION(:), allocatable :: Acceleration	!Acceleration array
  LOGICAL, DIMENSION(:), allocatable :: PlugArray	!Has value 1 for all layers that are part of a plug and 0 for all other layers
  REAL, DIMENSION(:), allocatable :: rho					!Density array

end module davinci_0_arrays
