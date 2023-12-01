#!/bin/bash

    if (make > trace_compilation.txt) then 

    echo "La compilation a reussi ! On peut lancer l'exécution"

    ./test.exe


    dot -Tpng tas_tab.dot -o Images/tas_tab.png
    dot -Tpng tas_arbre.dot -o Images/tas_arbre.png

    else 
        echo "Attention ! Vous n'avez peut-être pas la bonne version de OCaml, il faut utiliser la version 4.14."
    fi