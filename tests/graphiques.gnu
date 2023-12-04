reset 
set terminal png
set output "../Images/courbes/cplxt_arbre.png"
set xlabel "taille de l'échantillon (nombre de clef)"
set ylabel "Temps moyen (sur 5 itérations)"
plot "complexite_ajout_arbre.txt" using 1:2 with linespoints title "ajout itératif", "complexite_cons_arbre.txt" using 1:2 with linespoints title "construction"



reset 
set terminal png
set output "../Images/courbes/cplxt_tab.png"
set xlabel "taille de l'échantillon (nombre de clef)"
set ylabel "Temps moyen (sur 5 itérations)"
plot "complexite_ajout_tab.txt" using 1:2 with linespoints title "ajout itératif", "complexite_cons_tab.txt" using 1:2 with linespoints title "construction"


reset 
set terminal png
set output "../Images/courbes/cplxt_cons.png"
set xlabel "taille de l'échantillon (nombre de clef)"
set ylabel "Temps moyen (sur 5 itérations)"
plot "complexite_cons_tab.txt" using 1:2 with linespoints title "tableau", "complexite_cons_arbre.txt" using 1:2 with linespoints title "arbre"



reset 
set terminal png
set output "../Images/courbes/cplxt_union.png"
set xlabel "taille de l'échantillon (nombre de clef)"
set ylabel "Temps moyen (sur 20 itérations)"
plot "complexite_union_arbre.txt" using 1:2 with linespoints title "union par arbre", "complexite_union_tab.txt" using 1:2 with linespoints title "union par tableau"


reset 
set terminal png
set output "../Images/courbes/cplxt_union_file.png"
set xlabel "taille de l'échantillon (nombre de clef)"
set ylabel "Temps moyen (sur 20 itérations)"
plot "complexite_union_file.txt" using 1:2 with linespoints title "union par file binomiale"



reset 
set terminal png
set output "../Images/courbes/cplxt_binomiale.png"
set xlabel "taille de l'échantillon (nombre de clef)"
set ylabel "Temps moyen (sur 20 itérations)"
plot "complexite_cons_file.txt" using 1:2 with linespoints title "construction file binomiale"


reset
set terminal png
set output "../Images/courbes/cplxt_cons_tab_regression.png"
set xlabel "taille de l'échantillon (nombre de clef)"
set ylabel "Temps moyen (sur 5 itérations)"
f (x) = m*x +b
fit f(x) "complexite_cons_tab.txt" using 1:2 via m,b
plot f(x) title "regression linéaire", "complexite_cons_tab.txt" using 1:2 title "construction par tableau" with points


reset
set terminal png
set output "../Images/courbes/cplxt_cons_arbre_regression.png"
set xlabel "taille de l'échantillon (nombre de clef)"
set ylabel "Temps moyen (sur 5 itérations)"
g (x) = m*x +b
fit g(x) "complexite_cons_arbre.txt" using 1:2 via m,b
plot g(x) title "regression linéaire", "complexite_cons_arbre.txt" using 1:2 title "construction par arbre" with points


reset
set terminal png
set output "../Images/courbes/cplxt_union_arbre_regression.png"
set xlabel "taille de l'échantillon (nombre de clef)"
set ylabel "Temps moyen (sur 5 itérations)"
h (x) = m*x +b
fit h(x) "complexite_union_arbre.txt" using 1:2 via m,b
plot h(x) title "regression linéaire", "complexite_union_arbre.txt" using 1:2 title "union par arbre" with points


reset
set terminal png
set output "../Images/courbes/cplxt_union_tab_regression.png"
set xlabel "taille de l'échantillon (nombre de clef)"
set ylabel "Temps moyen (sur 5 itérations)"
i(x) = m*x +b
fit i(x) "complexite_union_tab.txt" using 1:2 via m,b
plot i(x) title "regression linéaire", "complexite_union_tab.txt" using 1:2 title "union par tableau" with points
