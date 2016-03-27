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
    OPEN(6,FILE = 'velocity'//x//'.txt')
    OPEN(7,FILE = 'density'//x//'.txt')

    !Write velocity profile to results file
    write(6,*)
    do n=1, TotalLayers+2
      WRITE (6,"(F12.6,A2)", advance="no") v(n), ", "
    enddo

    !write density profile to file
    write(7,*)
    do n=1, TotalLayers+2
      WRITE (7,"(F12.6,A2)", advance="no") rho(n), ", "
    enddo

    !	CLOSE(6)
    !	CLOSE(7)

  end subroutine output
end module davinci_1_output
