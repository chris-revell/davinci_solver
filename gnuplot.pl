#Script to draw davinci system with gnuplot

use strict;
use warnings;

#Script requires 3 arguments: total number of fluid layers, total time and the folder name within /data for the output
-defined($ARGV[0]) or die "Missing arguments";
-defined($ARGV[1]) or die "Missing 2nd argument";
-defined($ARGV[2]) or die "Missing 2nd argument";

my $TotalLayers = $ARGV[0];
my $TotalTime   = $ARGV[1];
my $folder      = $ARGV[2];

#Create and open the gnuplot script file for writing
open(my $file_handle,">", "data/".$folder."/GnuplotCommands.gnu") or die "can't open file";

#Print terms to set up plots
my $yrange = $TotalLayers+1;
print $file_handle "set terminal png\n";
print $file_handle "set xlabel 'Velocity'\n";
print $file_handle "set ylabel 'Fluid height'\n";
print $file_handle "set yrange [ 0 : $yrange ]\n";
print $file_handle "set xrange [ 0 : 20 ]\n";
print $file_handle "unset key\n";


my $i = 0;
my $filenumber;
while ($i < 101) {
  my $lowerbound = $i*($TotalLayers+2);
  my $upperbound = ($i+1)*($TotalLayers+2)-1;
  $filenumber = sprintf("%03d",$i);
  print $file_handle "set output 'data/$folder/v$filenumber.png'\n";
  print $file_handle "plot 'data/$folder/velocity.txt' every ::",$lowerbound,"::",$upperbound," using 1:2 with points\n";
  $i++
}
print "\n";

print $file_handle "set terminal png\n";
print $file_handle "set xlabel 'Density'\n";
print $file_handle "set ylabel 'Fluid height'\n";
print $file_handle "set yrange [ 0 : $yrange ]\n";
print $file_handle "set xrange [ 6 : 7]\n";
print $file_handle "unset key\n";

$i = 0;
while ($i < 101) {
  my $lowerbound = $i*$TotalLayers;
  my $upperbound = ($i+1)*$TotalLayers-1;
  $filenumber = sprintf("%03d",$i);
  print $file_handle "set output 'data/$folder/d$filenumber.png'\n";
  print $file_handle "plot 'data/$folder/density.txt' every ::",$lowerbound,"::",$upperbound," using 1:2 with points\n";
  $i++
}
