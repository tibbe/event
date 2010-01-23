set terminal png font "/Library/Fonts/Arial.ttf"
set output "thread-delay.png"
set xlabel "threads"
set ylabel "time (s)"
set logscale y
plot "new.dat" title "new" with lines, "old.dat" title "old" with lines
