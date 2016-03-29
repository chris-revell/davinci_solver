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

    !Write velocity profile to results file, excluding upper and lower boundary
    do n=2, TotalLayers
      WRITE(6,"(F12.6,A2)", advance="no") v(n), ", "   !advance='no' keeps all data for this timestep on one line
    enddo
    write(6,"(F12.6)") v(TotalLayers+1) !Writing last data point with advance to start a newline after

    !Write density profile to results file
    do n=2, TotalLayers
      WRITE (7,"(F12.6,A2)", advance="no") rho(n), ", "   !advance='no' keeps all data for this timestep on one line
    enddo
    write(7,"(F12.6)") rho(TotalLayers+1) !Writing last data point with advance to start a newline after

    !	CLOSE(6)
    !	CLOSE(7)

  end subroutine output
end module davinci_1_output
