reset
set term jpeg
set output "wykres2.jpg"
set xlabel "U(V)
set ylabel "I(mA)"
set title "Charakterystyka I(U)"
set xrange [0.1:0.6]
set yrange [-0.5:2.2]
plot "dane2.txt" using 1:2 with lines

