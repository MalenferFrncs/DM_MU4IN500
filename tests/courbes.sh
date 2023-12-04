#!/bin/bash

    if (make test_temps.exe) then 

    echo "La compilation a reussi ! On peut lancer l'exécution"

    ./test_temps.exe

    gnuplot graphiques.gnu


    else 
        echo "Attention ! Vous n'avez peut-être pas la bonne version de OCaml, il faut utiliser la version 4.14."
    fi