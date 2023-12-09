
all : int128.ml manipulation_fichiers.ml tas_min_tab.ml tas_min_arbre.ml test_graphique.exe test_temps.exe

experimentation_shakespeare.exe : ../src/lib/int128.ml ../src/lib/manipulation_fichiers.ml ../src/lib/tas_min_tab.ml  ../src/lib/tas_min_arbre.ml ../src/lib/arbre_234.ml ../src/lib/file_binomiale.ml ../src/lib/md5.ml liste_mots.ml experimentation_shakespeare.ml
	ocamlopt -I ../src/lib ../src/lib/int128.ml ../src/lib/manipulation_fichiers.ml ../src/lib/tas_min_tab.ml  ../src/lib/tas_min_arbre.ml ../src/lib/arbre_234.ml ../src/lib/file_binomiale.ml ../src/lib/md5.ml liste_mots.ml experimentation_shakespeare.ml -o experimentation_shakespeare.exe

test_graphique.exe : ../src/lib/int128.ml ../src/lib/manipulation_fichiers.ml ../src/lib/tas_min_tab.ml  ../src/lib/tas_min_arbre.ml ../src/lib/arbre_234.ml ../src/lib/file_binomiale.ml test_graphique.ml
	ocamlopt -I ../src/lib ../src/lib/int128.ml ../src/lib/manipulation_fichiers.ml ../src/lib/tas_min_tab.ml  ../src/lib/tas_min_arbre.ml ../src/lib/arbre_234.ml ../src/lib/file_binomiale.ml test_graphique.ml -o test_graphique.exe


test_temps.exe : ../src/lib/int128.ml ../src/lib/manipulation_fichiers.ml ../src/lib/tas_min_tab.ml  ../src/lib/tas_min_arbre.ml ../src/lib/arbre_234.ml ../src/lib/file_binomiale.ml test_temps.ml
	ocamlopt -I ../src/lib ../src/lib/int128.ml ../src/lib/manipulation_fichiers.ml ../src/lib/tas_min_tab.ml  ../src/lib/tas_min_arbre.ml ../src/lib/arbre_234.ml ../src/lib/file_binomiale.ml test_temps.ml -o test_temps.exe


clean : 
	rm -f *.cmo *.cmi *.exe *.dot *.cmx *.o *.txt *.out



