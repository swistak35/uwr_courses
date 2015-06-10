set terminal png size 640,480 enhanced font "Helvetica,10"
set output 'big_comp.png'
set xtics 1
set xlabel "Chunk size (MB)"
set ylabel "Compression time (ms)"
plot "big_comp.data" using 1:2 title 'rfc3' with lines, \
     "big_comp.data" using 1:3 title 'rfc4' with lines

