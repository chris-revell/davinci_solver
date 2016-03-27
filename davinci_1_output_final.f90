!CK Revell, March 2016
!Module containing subroutine to output simulation parameters to file

module davinci_1_output_final

  use davinci_0_variables

  implicit none

contains

  subroutine output_final

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

  end subroutine output_final

end module davinci_1_output_final
