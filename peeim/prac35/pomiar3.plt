reset
set term jpeg
set output "wykres3.jpg"
set xlabel "U(V)
set ylabel "I(mA)"
set title "Charakterystyka I(U)"
set xrange [-1:4]
set yrange [-10.0:10.2]
plot "dane3.txt" using 1:2 with lines

