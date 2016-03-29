!CK Revell, March 2016
!Module containing subroutine to output gnuplot command file

module davinci_1_gnuplot

	use davinci_0_variables

contains

	subroutine gnuplot_output

		!Create gnuplot command files for both fig and png outputs
		OPEN(3, FILE = "data/"//output_folder//"/GnuplotCommands.gnu")
		WRITE (3,*) 'set terminal png'
		WRITE (3,*) 'set output "data/'//output_folder//'/DensityPlot.png"'
		WRITE (3,*) 'set xlabel "Fluid height (n)"'
		WRITE (3,"(A17,I4,A24)") 'set xrange [ 0 : ', TotalLayers, ' ] noreverse nowriteback'
		WRITE (3,*) 'set xlabel  offset character 3, -2, 0 font "" textcolor lt -1 rotate by 90'
		WRITE (3,*) 'set ylabel "Time"'
		WRITE (3,*) 'set ylabel  offset character -3, -2, 0 font "" textcolor lt -1 norotate'
		WRITE (3,"(A17,I4,A24)") 'set yrange [ 0 : ', TotalTime, ' ] noreverse nowriteback'
		WRITE (3,*) 'set zlabel "Velocity"'
		WRITE (3,*) 'set zlabel  offset character -2, 0, 0 font "" textcolor lt -1 norotate'
		WRITE (3,*) 'splot "data/'//output_folder//'/density.txt" matrix with pm3d  t  "Evolution of velocity profile with time"'

		WRITE	(3,*) ""

		WRITE (3,*) 'set terminal png'
		WRITE (3,*) 'set output "data/'//output_folder//'/VelocityPlot.png"'
		WRITE (3,*) 'set xlabel "Fluid height (n)"'
		WRITE (3,"(A17,I4,A24)") 'set xrange [ 0 : ', TotalLayers, ' ] noreverse nowriteback'
		WRITE (3,*) 'set xlabel  offset character 3, -2, 0 font "" textcolor lt -1 rotate by 90'
		WRITE (3,*) 'set ylabel "Time"'
		WRITE (3,*) 'set ylabel  offset character -3, -2, 0 font "" textcolor lt -1 norotate'
		WRITE (3,"(A17,I4,A24)") 'set yrange [ 0 : ', TotalTime, ' ] noreverse nowriteback'
		WRITE (3,*) 'set zlabel "Velocity"'
		WRITE (3,*) 'set zlabel  offset character -2, 0, 0 font "" textcolor lt -1 norotate'
		WRITE (3,*) 'splot "data/'//output_folder//'/velocity.txt" matrix with pm3d  t  "Evolution of velocity profile with time"'
		CLOSE(3)

		!Run gnuplot using fortran system call
		call system("chmod u+x data/"//output_folder//"/GnuplotCommands.gnu") !Have to allow permission to use the script, not sure how to change default permissions for new scripts.
		call system("gnuplot 'data/"//output_folder//"/GnuplotCommands.gnu'")

	end subroutine gnuplot_output
end module davinci_1_gnuplot
