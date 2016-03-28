!CK Revell, March 2016
!Module containing subroutine to output simulation parameters to file

module davinci_1_output_final

  use davinci_0_variables

  implicit none

contains

  subroutine output_final

    !Create file to store results parameters
    OPEN(4, FILE = "data/"//output_folder//"/Parameters.txt")

!**********************************************************************
!    IF (FileOrFunction.EQ.1) THEN
!      WRITE(4,*) "Input filename: "//InputFilename//".txt"
!    ELSE
!      WRITE(4,*) "Initial velocity profile function choice:"
!      WRITE(4,*) FunctionChoice
!    END IF
!*********************************************************************

    WRITE(4,*) "FunctionChoice:", FunctionChoice
    WRITE(4,*) "Total time:", TotalTime
    WRITE(4,*) "Total number of fluid layers:", TotalLayers
    WRITE(4,*) "dt:", dt
    WRITE(4,*) "Upper boundary pressure P_0:", P_0
    WRITE(4,*) "Static friction coefficient:", mu_2
    WRITE(4,*) "Accuracy, epsilon:", epsilon
    WRITE(4,*) "Layer thickness, d:", d
    WRITE(4,*) "Alpha:", alpha
    WRITE(4,*) "Loose random packing, rho_0:", rho_0
    CLOSE(4)

  end subroutine output_final

end module davinci_1_output_final
