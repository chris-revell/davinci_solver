!CK Revell, March 2016
!Module containing subroutine to output gnuplot command file

module davinci_1_gnuplot

	use davinci_0_variables

contains

	subroutine gnuplot_output

		INTEGER :: i

		OPEN(3, FILE = "data/"//output_folder//"/GnuplotCommands.gnu")
		WRITE (3,*) 'set terminal png'
		WRITE (3,*) 'set xlabel "Fluid height"'
		WRITE (3,*) 'set ylabel "Velocity"'
		WRITE (3,"(A17,I4.4,A1)") 'set xrange [ 0 : ', TotalLayers, ']'
!		WRITE (3,"(A17,I4,A24)") 'set yrange [ 0 : ', ???, ' ]'
!		WRITE (3,*) 'set xlabel  offset character 3, -2, 0 font "" textcolor lt -1 rotate by 90'
!		WRITE (3,*) 'set ylabel  offset character -3, -2, 0 font "" textcolor lt -1 norotate'
!		WRITE (3,*) 'set zlabel "Velocity"'
!		WRITE (3,*) 'set zlabel  offset character -2, 0, 0 font "" textcolor lt -1 norotate'
!		WRITE (3,*) 'splot "data/'//output_folder//'/density.txt" matrix with pm3d  t  "Evolution of velocity profile with time"'
		DO i=0, TotalTime
			IF (i.LT.10) THEN
				WRITE (3,"(A31,I1,A5)") 'set output "data/'//output_folder//'/', i, '.png"'
				WRITE (3,"(A47,I1,A2,I1,A21)") 'plot "data/'//output_folder//'/velocity.txt" every ::', &
					i*TotalLayers+1, '::', (i+1)*TotalLayers, ' using 1:2 with lines'
			ELSEIF (i.LT.100) THEN
				WRITE (3,"(A31,I2,A5)") 'set output "data/'//output_folder//'/', i, '.png"'
				WRITE (3,"(A47,I2,A2,I2,A21)") 'plot "data/'//output_folder//'/velocity.txt" every ::', &
					i*TotalLayers+1, '::', (i+1)*TotalLayers, ' using 1:2 with lines'
			ELSEIF (i.LT.1000) THEN
				WRITE (3,"(A31,I3,A5)") 'set output "data/'//output_folder//'/', i, '.png"'
				WRITE (3,"(A47,I3,A2,I3,A21)") 'plot "data/'//output_folder//'/velocity.txt" every ::', &
					i*TotalLayers+1, '::', (i+1)*TotalLayers, ' using 1:2 with lines'
			ELSE
				WRITE (3,"(A31,I4,A5)") 'set output "data/'//output_folder//'/', i, '.png"'
				WRITE (3,"(A47,I4,A2,I4,A21)") 'plot "data/'//output_folder//'/velocity.txt" every ::', &
					i*TotalLayers+1, '::', (i+1)*TotalLayers, ' using 1:2 with lines'
			ENDIF
		ENDDO

		!Run gnuplot using fortran system call and convery images to animated gif
		call system("chmod u+x data/"//output_folder//"/GnuplotCommands.gnu") !Have to allow permission to use the script, not sure how to change default permissions for new scripts.
		call system("gnuplot 'data/"//output_folder//"/GnuplotCommands.gnu'")
		call system("convert -delay 20 -loop 0 *.png velocity_animated.gif")
		call system("rm *.png")


!		WRITE	(3,*) ""

!		WRITE (3,*) 'set terminal png'
!		WRITE (3,*) 'set output "data/'//output_folder//'/VelocityPlot.png"'
!		WRITE (3,*) 'set xlabel "Fluid height (n)"'
!		WRITE (3,"(A17,I4,A24)") 'set xrange [ 0 : ', TotalLayers, ' ] noreverse nowriteback'
!		WRITE (3,*) 'set xlabel  offset character 3, -2, 0 font "" textcolor lt -1 rotate by 90'
!		WRITE (3,*) 'set ylabel "Time"'
!		WRITE (3,*) 'set ylabel  offset character -3, -2, 0 font "" textcolor lt -1 norotate'
!		WRITE (3,"(A17,I4,A24)") 'set yrange [ 0 : ', TotalTime, ' ] noreverse nowriteback'
!		WRITE (3,*) 'set zlabel "Velocity"'
!		WRITE (3,*) 'set zlabel  offset character -2, 0, 0 font "" textcolor lt -1 norotate'
!		WRITE (3,*) 'splot "data/'//output_folder//'/velocity.txt" matrix with pm3d  t  "Evolution of velocity profile with time"'
		CLOSE(3)



	end subroutine gnuplot_output
end module davinci_1_gnuplot
