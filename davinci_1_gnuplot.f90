!CK Revell, March 2016
!Module containing subroutine to output gnuplot command file

module davinci_1_gnuplot

	use davinci_0_variables

contains

	subroutine gnuplot_output

		!Output to command line the command that must now be entered into gnuplot in order to plot these results
		WRITE(*,*) "Copy the following lines into Gnuplot to plot these results"
		WRITE(*,*) "load 'data/RhoGnuplotCommandsPng"//x//".txt'"
!		WRITE(*,*) "load 'data/RhoGnuplotCommandsFig"//x//".txt'"

		!Create gnuplot command files for both fig and png outputs
		OPEN(3, FILE = "data/RhoGnuplotCommandsPng"//x//'.txt')
		WRITE (3,*) 'set terminal png'
		WRITE (3,*) 'set output "data/RhoPlot'//x//'.png"'
		WRITE (3,*) 'set xlabel "Fluid height (n)"'
		WRITE (3,"(A17,I4,A24)") 'set xrange [ 0 : ', TotalLayers+2, ' ] noreverse nowriteback'
		WRITE (3,*) 'set xlabel  offset character 3, -2, 0 font "" textcolor lt -1 rotate by 90'
		WRITE (3,*) 'set ylabel "Time"'
		WRITE (3,*) 'set ylabel  offset character -3, -2, 0 font "" textcolor lt -1 norotate'
		WRITE (3,"(A17,I4,A24)") 'set yrange [ 0 : ', TotalTime, ' ] noreverse nowriteback'
		WRITE (3,*) 'set zlabel "Velocity"'
		WRITE (3,*) 'set zlabel  offset character -2, 0, 0 font "" textcolor lt -1 norotate'
		WRITE (3,*) 'splot "data/RhoResults'//x//'.txt" matrix with pm3d  t  "Evolution of velocity profile with time"'
		CLOSE(3)
		!OPEN(2, FILE = "RhoGnuplotCommandsFig"//x//'.txt')
		!WRITE (2,*) 'set terminal fig color'
		!WRITE (2,*) 'set output "RhoPlot'//x//'.fig"'
		!WRITE (2,*) 'set xlabel "Fluid height (n)"'
		!WRITE (2,*) 'set xrange [ 0 : '//Layerbound//' ] noreverse nowriteback'
		!WRITE (2,*) 'set xlabel  offset character 3, -2, 0 font "" textcolor lt -1 rotate by 90'
		!WRITE (2,*) 'set ylabel "Time"'
		!WRITE (2,*) 'set ylabel  offset character -3, -2, 0 font "" textcolor lt -1 norotate'
		!WRITE (2,*) 'set yrange [ 0 : '//Timebound//' ] noreverse nowriteback'
		!WRITE (2,*) 'set zlabel "Velocity"'
		!WRITE (2,*) 'set zlabel  offset character -2, 0, 0 font "" textcolor lt -1 norotate'
		!WRITE (2,*) 'splot "RhoResults'//x//'.txt" matrix with pm3d  t  "Evolution of velocity profile with time"'
		!CLOSE(2)
	end subroutine gnuplot_output
end module davinci_1_gnuplot
