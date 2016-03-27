#Script to compile modules of davinci solver and combine to form main program

gfortran -c davinci_0_*.f90 -fbounds-check
gfortran -c davinci_1_*.f90 -fbounds-check
gfortran davinci_0_*.f90 davinci_1_*.f90 SOLVER_rho.f90 -o SOLVER_rho
rm *.o
rm *.mod
