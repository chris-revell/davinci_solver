#Script to draw davinci system with gnuplot

use strict;
use warnings;

-defined($ARGV[0]) or die "Missing arguments";
-defined($ARGV[1]) or die "Missing 2nd argument";
-defined($ARGV[2]) or die "Missing 2nd argument";

my $TotalLayers = $ARGV[0];
my $TotalTime   = $ARGV[1];
my $folder      = $ARGV[2];

open(my $file_handle,'>', "data/".$folder."/GnuplotCommands.gnu") or die "can't open file";

print $file_handle "set terminal png\n";
print $file_handle "set xlabel 'Fluid height'\n";
print $file_handle "set ylabel 'Velocity'\n";
print $file_handle "set xrange [ 1 : $TotalLayers ]\n";

my $i = 0;
while ($i <= $TotalTime) {
  my $lowerbound = $i*$TotalLayers;
  my $upperbound = ($i+1)*$TotalLayers-1;
  print $file_handle "set output 'data/$folder/v$i.png'\n";
  print $file_handle "plot 'data/$folder/velocity.txt' every ::",$lowerbound,"::",$upperbound," using 1:2 with lines\n";
  $i++
}
print "\n";

print $file_handle "set terminal png\n";
print $file_handle "set xlabel 'Fluid height'\n";
print $file_handle "set ylabel 'Density'\n";
print $file_handle "set xrange [ 1 : $TotalLayers]\n";

$i = 0;
while ($i <= $TotalTime) {
  my $lowerbound = $i*$TotalLayers;
  my $upperbound = ($i+1)*$TotalLayers-1;
  print $file_handle "set output 'data/$folder/d$i.png'\n";
  print $file_handle "plot 'data/$folder/velocity.txt' every ::",$lowerbound,"::",$upperbound," using 1:2 with lines\n";
  $i++
}
