set terminal png size 640,480 enhanced font "Helvetica,10"
set output 'rfc_comp.png'
set xtics 1
set xlabel "Chunk size (100KB)"
set ylabel "Compression time (ms)"
plot "rfc_comp.data" using 1:10 title 'suffix' with lines, \
     "rfc_comp.data" using 1:19 title 'uni' with lines


