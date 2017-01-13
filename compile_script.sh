#Script to compile modules of davinci solver and combine to form main program

gfortran -c modules/davinci_0_*.f90 -fbounds-check
gfortran -c modules/davinci_1_*.f90 -fbounds-check
gfortran -c modules/davinci_2_*.f90 -fbounds-check
gfortran modules/davinci_0_*.f90 modules/davinci_1_*.f90 modules/davinci_2_*.f90 modules/davinci_solver.f90 -o davinci_solver
rm *.o
rm *.mod
