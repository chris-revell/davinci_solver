!CK Revell, March 2016
!Module containing subroutine to output data from da vinci solver

module davinci_1_output

  use davinci_0_arrays
  use davinci_0_variables

  implicit none

contains

  subroutine output

    integer :: n

    !Open file for results
    OPEN(6,FILE = 'data/'//output_folder//'/velocity.txt', status='unknown')
    OPEN(7,FILE = 'data/'//output_folder//'/density.txt', status='unknown')

    !Write velocity profile to results file, excluding upper and lower boundary. Whole system at each timestep written as one long column for ease of plotting.
    do n=2, TotalLayers+1
      WRITE(6, "(F12.6,A2,I4)") v(n), ", ", n-1
    enddo

    !Write density profile to results file. Whole system at each timestep written as one long column for ease of plotting.
    do n=2, TotalLayers+1
      WRITE(7, "(F12.6,A2,I4)") rho(n), ", ", n-1
    enddo

  end subroutine output
end module davinci_1_output
