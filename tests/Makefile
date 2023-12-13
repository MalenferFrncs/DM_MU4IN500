CO = ocamlopt
OFLAGS = -I $(REPLIB)
REPLIB = ../src/lib/
REPGRAPHE = ../Images/graphes/
REPCOURBE = ../Images/courbes/
REPSHAKESPEARE = ../Images/courbes_shakespeare/
VAL_TEST_GRAPHIQUE = "../src/jeux_de_données/cles_alea/jeu_1_nb_cles_1000.txt" "15"
REPDST = 
SRC_LIB = int128.ml tas_min_tab.ml tas_min_arbre.ml file_binomiale.ml md5.ml arbre_234.ml manipulation_fichiers.ml
SRC_TEST = liste_mots.ml experimentation_shakespeare.ml test_graphique.ml test_temps.ml 

OBJ_LIB = $(SRC_LIB:.ml=.cmx)



all: test_all

test_all: experimentation_shakespeare test_graphique test_temps



test_graphique : test_graphique.exe 
	echo "on lance test graphique"
	./test_graphique.exe $(VAL_TEST_GRAPHIQUE)

	dot -Tpng tas_tab_cons.dot -o $(REPGRAPHE)/tas_tab_cons.png
	dot -Tpng tas_tab_ajouts.dot -o $(REPGRAPHE)/tas_tab_ajouts.png
	dot -Tpng tas_tab_ajouts_supprMin.dot -o $(REPGRAPHE)/tas_tab_ajouts_supprMin.png
	dot -Tpng tas_arbre_cons.dot -o $(REPGRAPHE)/tas_arbre_cons.png
	dot -Tpng tas_arbre_ajouts.dot -o $(REPGRAPHE)/tas_arbre_ajouts.png
	dot -Tpng file_binomiale_cons.dot -o $(REPGRAPHE)/file_binomiale.png
	dot -Tpng arbre_234_ajout.dot -o $(REPGRAPHE)/arbre_234.png
	dot -Tpng tas_arbre_ajouts_supprMin.dot -o $(REPGRAPHE)tas_arbre_ajouts_supprMin.png

test_temps : test_temps.exe 
	./test_temps.exe 
	gnuplot graphiques.gnu


experimentation_shakespeare : experimentation_shakespeare.exe 
	./experimentation_shakespeare.exe
	gnuplot experimentation_shakespeare.gnu

%.exe : %.ml liste_mots
	$(CO) $(OFLAGS) $(OBJ_LIB) liste_mots.cmx $< -o $@

liste_mots : liste_mots.ml lib
	$(CO) $(OFLAGS) -c $(OBJ_LIB) liste_mots.ml 
	

lib :
	cd $(REPLIB) && $(MAKE) all


$(REPDST)%.cmx: $(REPSRC)%.ml 
	$(CO) $(OFLAGS) -c $< 

clean_graphe :
	rm -f $(REPGRAPHE)*.png

clean_courbes : 
	rm -f $(REPCOURBE)*.png

clean_shakespeare :
	rm -f $(REPSHAKESPEARE)*.png

clean_test : clean_courbes clean_graphe clean_shakespeare 

clean_lib :
	cd $(REPLIB) && $(MAKE) clean

clean :
	rm -f *.o *.cmi *.cmx *.exe *.out *.dot *.txt

.PHONY :clean_lib clean 