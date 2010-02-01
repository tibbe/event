set terminal png font "/Library/Fonts/Arial.ttf"
set output "timers.png"
set xlabel "threads"
set ylabel "time (s)"
plot "events.dat" title "event loop" with lines, "new.dat" title "threads" with lines
