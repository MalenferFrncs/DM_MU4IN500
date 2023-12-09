

reset
set terminal png
set output "../Images/courbes/temps_construction_shakespeare.png"
set boxwidth 0.5
set style fill solid 0.20 border 
set grid ytics linestyle 0
set style data histograms
set ylabel "temps (secondes)"
set yrange [0:0.01]
set style histogram rowstacked
plot 'construction_shakespeare.txt' using 2:xtic(1) title "construction"


reset
set terminal png
set output "../Images/courbes/temps_suppr_shakespeare.png"
set boxwidth 0.5
set style fill solid 0.20 border 
set grid ytics linestyle 0
set style data histograms
set ylabel "temps (secondes)"
set yrange [0:0.000015]
set style histogram rowstacked
plot 'suppr_Min_shakespeare.txt' using 2:xtic(1) title "Suppression du minimum"

reset
set terminal png
set output "../Images/courbes/temps_ajout_shakespeare.png"
set boxwidth 0.5
set style fill solid 0.20 border 
set grid ytics linestyle 0
set style data histograms
set ylabel "temps (secondes)"
set yrange [0:0.00001]
set style histogram rowstacked
plot 'ajout_shakespeare.txt' using 2:xtic(1) title "Ajout de l'élément minimum"

reset
set terminal png
set output "../Images/courbes/temps_union_shakespeare.png"
set boxwidth 0.5
set style fill solid 0.20 border 
set grid ytics linestyle 0
set style data histograms
set ylabel "temps moyen sur 20 itérations(secondes)"
set yrange [0:0.006]
set style histogram rowstacked
plot 'union_shakespeare.txt' using 2:xtic(1) title "Union de deux files construites aléatoirement"