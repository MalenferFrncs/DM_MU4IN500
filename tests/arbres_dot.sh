#!/bin/bash

    if (make test_graphique.exe) then 

    echo "La compilation a reussi ! On peut lancer l'exécution"

    ./test_graphique.exe


    dot -Tpng tas_tab_cons.dot -o ../Images/graphes/tas_tab_cons.png
    dot -Tpng tas_tab_ajouts.dot -o ../Images/graphes/tas_tab_ajouts.png
    dot -Tpng tas_tab_ajouts_supprMin.dot -o ../Images/graphes/tas_tab_ajouts_supprMin.png
    dot -Tpng tas_arbre_cons.dot -o ../Images/graphes/tas_arbre_cons.png
    dot -Tpng tas_arbre_ajouts.dot -o ../Images/graphes/tas_arbre_ajouts.png
    dot -Tpng tas_arbre_ajouts_supprMin.dot -o ../Images/tas_arbre_ajouts_supprMin.png
    dot -Tpng file_binomiale_cons.dot -o ../Images/graphes/file_binomiale.png
    dot -Tpng arbre_234_ajout.dot -o ../Images/graphes/arbre_234.png


    make clean
    else 
        echo "Attention ! Vous n'avez peut-être pas la bonne version de OCaml, il faut utiliser la version 4.14."
    fi