#!/bin/bash

    if (ocamlopt int128.ml tas_min_arbre.ml tas_min_tab.ml file_binomiale.ml manipulation_fichiers.ml test_graphique.ml -o test_graphique.exe) then 

    echo "La compilation a reussi ! On peut lancer l'exécution"

    ./test_graphique.exe


    dot -Tpng tas_tab_cons.dot -o Images/tas_tab_cons.png
    dot -Tpng tas_tab_ajouts.dot -o Images/tas_tab_ajouts.png
    dot -Tpng tas_tab_ajouts_supprMin.dot -o Images/tas_tab_ajouts_supprMin.png

    dot -Tpng tas_arbre_cons.dot -o Images/tas_arbre_cons.png
    dot -Tpng tas_arbre_ajouts.dot -o Images/tas_arbre_ajouts.png
    dot -Tpng tas_arbre_ajouts_supprMin.dot -o Images/tas_arbre_ajouts_supprMin.png


    make clean
    else 
        echo "Attention ! Vous n'avez peut-être pas la bonne version de OCaml, il faut utiliser la version 4.14."
    fi