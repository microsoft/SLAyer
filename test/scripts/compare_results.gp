# columns of data are tab-separated
set datafile separator "	"

# stats doesn't work with logscale
unset logscale
# compute STATS_max for use below
stats "RESULT.compare.tsv" using "Time1" nooutput

set logscale xy 10
set xtics 0.1
set ytics 0.1
set size square
set key left top Left

plot [1:STATS_max][1:STATS_max] \
  x/0.001 title "y = 1000x" with lines lt 6, \
  x/0.01 title "y = 100x" with lines lt 7, \
  x/0.1 title "y = 10x" with lines lt 8, \
  x/0.5 title "y = 2x" with lines lt 9, \
  x/(1/1.1) title "y = 1.1x" with lines lt 10, \
  x/1.1 title "1.1y = x" with lines lt 1, \
  x/2 title "2y = x" with lines lt 2, \
  x/10 title "10y = x" with lines lt 3, \
  x/100 title "100y = x" with lines lt 4, \
  x/1000 title "1000y = x" with lines lt 5, \
  "RESULT.compare.tsv" using \
    ((column("Time1")) < 1 ? 1 : (column("Time1"))): \
    ((column("Time2")) < 1 ? 1 : (column("Time2"))) \
  with points pt 2 lc 'black' notitle


# # to resize points to have area proportional to the x coordinate:
#   "RESULT.merge.tsv" using \
#     ((column("Time1")) < 1 ? 1 : (column("Time1"))): \
#     ((column("Time2")) < 1 ? 1 : (column("Time2"))): \
#     (sqrt(column("Time1"))/sqrt(STATS_max)) \
#   with points pt 2 lc 'black' ps variable notitle
