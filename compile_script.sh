#Script to compile modules of davinci solver and combine to form main program

gfortran -c davinci_solver/davinci_0_*.f90 -fbounds-check
gfortran -c davinci_solver/davinci_1_*.f90 -fbounds-check
gfortran -c davinci_solver/davinci_2_*.f90 -fbounds-check
gfortran davinci_solver/davinci_0_*.f90 davinci_solver/davinci_1_*.f90 davinci_solver/davinci_2_*.f90 davinci_solver/davinci_solver.f90 -o master_davinci
rm *.o
rm *.mod
