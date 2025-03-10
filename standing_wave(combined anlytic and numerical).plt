set terminal gif animate delay 15
set output "standing_wave.gif"
stats "wave.txt" name "wave"
set xrange[wave_min_x:wave_max_x]
set yrange[wave_min_y:wave_max_y]
set xlabel "Length(x)"
set ylabel "Displacement(u)"
do for[i=0:wave_blocks-2]{plot "wave.txt" index i w l title sprintf("time = %.3f", i*0.005)}

set terminal gif animate delay 15
set output "wave_a.gif"
stats "wave_a.txt" name "wave"
set xrange[wave_min_x:wave_max_x]
set yrange[wave_min_y:wave_max_y]
set xlabel "Length(x)"
set ylabel "Displacement(u)"
do for[i=0:wave_blocks-2]{plot "wave_a.txt" index i w l title sprintf("time = %.3f", i*0.005)}