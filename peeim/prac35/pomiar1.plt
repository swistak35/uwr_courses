reset
set term jpeg
set output "wykres1.jpg"
set xlabel "U(V)
set ylabel "I(mA)"
set title "Charakterystyka I(U)"
set xrange [1.6:1.9]
set yrange [-0.5:4.0]
plot "dane1.txt" using 1:2 with lines

