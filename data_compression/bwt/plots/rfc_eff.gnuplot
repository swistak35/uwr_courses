set terminal png size 640,480 enhanced font "Helvetica,10"
set output 'rfc_eff.png'
set xtics 1
set xlabel "Chunk size (100KB)"
set ylabel "Compression level (in %, the lower the better)"
plot "rfc_eff.data" using 1:10 title 'Efficiency' with lines

