!CK Revell, March 2016
!Module containing subroutine to output data from da vinci solver

module davinci_1_output

  use davinci_0_arrays
  use davinci_0_variables

  implicit none

contains

  subroutine output

    integer :: p

    !Write velocity profile to results file, excluding upper and lower boundary. Whole system at each timestep written as one long column for ease of plotting.
    OPEN(unit=10,FILE = '../data/'//output_folder//'/velocity.txt', access="append")
    do p=1, TotalLayers+2
      WRITE(10, "(F12.6,A2,I4)") v(p), ", ", p-1
    enddo
    CLOSE(10)

    !Write density profile to results file. Whole system at each timestep written as one long column for ease of plotting.
    OPEN(unit=10,FILE = '../data/'//output_folder//'/density.txt', access="append")
    do p=2, TotalLayers+1
      WRITE(10, "(F12.6,A2,I4)") rho(p), ", ", p-1
    enddo
    close(10)

  end subroutine output
end module davinci_1_output
