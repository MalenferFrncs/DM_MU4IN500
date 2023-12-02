reset 
set terminal png
set output "Images/cplxt_arbre.png"
set xlabel "taille de l'échantillon (nombre de clef)"
set ylabel "Temps moyen (sur 5 itérations)"
plot "complexite_ajout_arbre.txt" using 1:2 with linespoints title "ajout itératif", "complexite_cons_arbre.txt" using 1:2 with linespoints title "construction"



reset 
set terminal png
set output "Images/cplxt_tab.png"
set xlabel "taille de l'échantillon (nombre de clef)"
set ylabel "Temps moyen (sur 5 itérations)"
plot "complexite_ajout_tab.txt" using 1:2 with linespoints title "ajout itératif", "complexite_cons_tab.txt" using 1:2 with linespoints title "construction"


reset 
set terminal png
set output "Images/cplxt_cons.png"
set xlabel "taille de l'échantillon (nombre de clef)"
set ylabel "Temps moyen (sur 5 itérations)"
plot "complexite_cons_tab.txt" using 1:2 with linespoints title "tableau", "complexite_cons_arbre.txt" using 1:2 with linespoints title "arbre"



reset 
set terminal png
set output "Images/cplxt_union.png"
set xlabel "taille de l'échantillon (nombre de clef)"
set ylabel "Temps moyen (sur 20 itérations)"
plot "complexite_union_arbre.txt" using 1:2 with linespoints title "union par arbre", "complexite_union_tab.txt" using 1:2 with linespoints title "union par tableau"


reset 
set terminal png
set output "Images/cplxt_binomiale.png"
set xlabel "taille de l'échantillon (nombre de clef)"
set ylabel "Temps moyen (sur 20 itérations)"
plot "complexite_cons_file.txt" using 1:2 with linespoints title "construction file binomiale"
