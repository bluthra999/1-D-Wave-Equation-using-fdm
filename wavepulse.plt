set terminal gif animate delay 5 optimize
set output "wavepulse.gif"
stats "wavepulse.txt" name "wavepulse"
set xrange[wavepulse_min_x:wavepulse_max_x]
set yrange[wavepulse_min_y:wavepulse_max_y]
set xlabel "Length(x)"
set ylabel "Displacement(u)"
do for[i=0:wavepulse_blocks-2]{plot "wavepulse.txt" index i w l }
