# DM_MU4IN500

## Sujet 
Le but du problème consiste à visualiser graphiquement les temps d’exécution sur des données
réelles des algorithmes et structures de données vues en cours  (tas minimum, file binomiale, arborescence de recherche, sous forme d'arbre 234).

## Organisation du répertoire

DM_MU4IN500
├── Rapport.pdf
├── README.md
├── LICENSE
├── Transparents.pdf
├── Images
│   ├── courbes
│   ├── courbes_shakespeare
│   ├── courbes utilisées pour le rapport
│   └── graphes
├── src
│   ├── jeux_de_données
│   │	├── cles_alea
│   │	└── Shakespeare
│   └── lib
│   	├── arbre_234.ml
│   	├── file_binomiale.ml
│   	├── int128.ml
│   	├── manipulation_fichiers.ml
│   	├── Makefile
│   	├── md5.ml
│   	├── tas_min_arbre.ml
│   	└── tas_min_tab.ml
└── test
    ├── experimentation_shakespeare.gnu
    ├── experimentation_shakespeare.ml
    ├── fit.log
    ├── graphiques.gnu
    ├── liste_mots.ml
    ├── Makefile
    ├── test_temps.ml
    └── test_graphique.ml

## Tests

Pour lancer les différentes fonctions, il suffit de se rendre dans le répertoire test et d'ouvrir un terminal :

* Pour obtenir des représentations graphiques dans Images/graphes : make test_graphique
* Pour obtenir des mesures de complexité temporelles avec des jeux de données aléatoires : make test_temps
* Pour obtenir les mesures réalisées sur le jeu de données obtenu à partir l'œuvre de Shakespeare : make exeperimentation_shakespeare
  

