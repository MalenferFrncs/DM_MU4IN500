
all : int128.cmo tas_min_tab.cmo tas_min_arbre.cmo test.exe


test.exe : int128.cmo tas_min_tab.cmo tas_min_arbre.cmo test.cmo
	ocamlc int128.cmo tas_min_tab.cmo tas_min_arbre.cmo test.cmo -o test.exe

test.cmo : 
	ocamlc -c test.ml

tas_min_tab.cmo : 
	ocamlc -c tas_min_tab.ml

tas_min_arbre.cmo :
	ocamlc -c tas_min_arbre.ml

int128.cmo : 
	ocamlc -c int128.ml


clean : 
	rm -f *.cmo *.cmi *.exe *.dot *.cmx *.o
