CO = ocamlopt
OFLAGS = 
REPSRC = 
REPDST = 
SRC = int128.ml tas_min_tab.ml tas_min_arbre.ml file_binomiale.ml md5.ml arbre_234.ml manipulation_fichiers.ml
OBJ = $(SRC : .ml = .cmx)



all: $(SRC)
	$(CO) $(OFLAGS) -c $(SRC) 

	 


$(REPDST)%.cmx: $(REPSRC)%.ml 
	$(CO) $(OFLAGS) -c $< 


clean :
	rm $(DESTSRC)*.cmi
	rm $(DESTSRC)*.cmx
	rm $(DESTSRC)*.o
	

.PHONY : clean 