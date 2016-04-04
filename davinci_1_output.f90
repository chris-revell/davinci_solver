!CK Revell, March 2016
!Module containing subroutine to output data from da vinci solver

module davinci_1_output

  use davinci_0_arrays
  use davinci_0_variables

  implicit none

contains

  subroutine output

    integer :: p
    !Open file for results

    !Write velocity profile to results file, excluding upper and lower boundary. Whole system at each timestep written as one long column for ease of plotting.
    write(*,*) "test"
    OPEN(6,FILE = 'data/'//output_folder//'/velocity.txt', access="append")
    do p=2, TotalLayers+1
      WRITE(6, "(F12.6,A2,I4)") v(p), ", ", p-1
    enddo
    CLOSE(6)



    !Write density profile to results file. Whole system at each timestep written as one long column for ease of plotting.
    OPEN(7,FILE = 'data/'//output_folder//'/density.txt', access="append")
    do p=2, TotalLayers+1
      WRITE(7, "(F12.6,A2,I4)") rho(p), ", ", p-1
    enddo
    close(7)

  end subroutine output
end module davinci_1_output
