
all : int128.cmo manipulation_fichiers.cmo tas_min_tab.cmo tas_min_arbre.cmo test_graphique.exe test_temps.exe


test_graphique.exe : int128.cmo manipulation_fichiers.cmo tas_min_tab.cmo tas_min_arbre.cmo test_graphique.cmo
	ocamlopt int128.cmo manipulation_fichiers.cmo tas_min_tab.cmo  tas_min_arbre.cmo test_graphique.cmo -o test_graphique.exe



test_temps.exe : int128.cmo manipulation_fichiers.cmo tas_min_tab.cmo tas_min_arbre.cmo test_temps.cmo
	ocamlopt int128.cmo manipulation_fichiers.cmo tas_min_tab.cmo tas_min_arbre.cmo test_temps.cmo -o test_temps.exe

test_temps.cmo : 
	ocamlopt -c test_temps.ml


test_graphique.cmo : 
	ocamlopt -c test_graphique.ml


tas_min_tab.cmo : 
	ocamlopt -c tas_min_tab.ml

tas_min_arbre.cmo :
	ocamlopt -c tas_min_arbre.ml


manipulation_fichiers.cmo : 
	ocamlopt -c manipulation_fichiers.ml

int128.cmo : 
	ocamlopt -c int128.ml


clean : 
	rm -f *.cmo *.cmi *.exe *.dot *.cmx *.o *.txt *.out
