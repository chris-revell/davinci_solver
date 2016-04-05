Note: program requires gfortran compiler, gnuplot and ImageMagick. On a mac, which this program was written on, one can install ImageMagick with MacPorts by running “sudo port install ImageMagick” in the command line.

Directory /davinci_solver contains all modules required for the da Vinci fluids solver and a compile script to compile and then combine all modules into a master program master_davinci.

The compile script must be run as ./davinci_solver/compile_script.sh from the director above davinci_solver. This parent directory must also contain a /data folder.

When running ./master_davinci a folder will be created within /data stamped with the date and time to contain all data output by the program. The program will automatically visualise the system using gnuplot and ImageMagick.

System parameters can be varied within the following modules:
- davinci_1_upperboundary.f90 to change how the speed of the upper system boundary varies over time
- davinci_1_lowerboundary.f90 to change how the speed of the lower system boundary varies over time
- davinci_1_initialvelocityfunction.f90 to change the velocity profile of the system at the start of a simulation
- davinci_1_densityrelation.f90 to change the functional form of the relationship between fluid density and velocity
- davinci_1_frictionrelation.f90 to change the functional form of the relationship between dynamic friction and fluid density
- davinci_2_initialise.f90 to change system parameters such as number of fluid layers, fluid thickness, external pressure , total time etc.

Currently needing work:
Overshooting terms may not be working for large time steps?
Is it even possible to make this work or do we just need to ensure timesteps are small enough that overshooting is impossible? 


No variation of static friction with density.

Dynamic friction and density relations need to be improved and justified.

Does plug_treatment work for a plug between the bottom boundary and the first fluid layer?
