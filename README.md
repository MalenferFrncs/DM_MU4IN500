# DM_MU4IN500

## Sujet 
Le but du problème consiste à visualiser graphiquement les temps d’exécution sur des données
réelles des algorithmes et structures de données vues en cours  (tas minimum, file binomiale, arborescence de recherche, sous forme d'arbre 234).

## Récupérer les jeux de données

Moodle nous limitant pour la taille de l'archive, nous n'avons pas pu inclure les fichiers des jeux de tests : pour effectuer nos expérimentations, il faut placer les deux dossiers dans src/jeux_de_données, en suivant l'arborescence de fichiers ci-dessous.

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

## Références

* Ernst E. Doberkat. An average case analysis of Foyd's algorithm to construct heaps. Infor-
mation and Control, 61(2) :114-131, 1984.
* Robert W. Floyd. Algorithm 245 : Treesort. Commun. ACM, 7(12) :701, dec 1964.
* Donald E. Knuth. The Art of Computer Programming, Volume 3 : (2nd Ed.) Sorting and
Searching, pages 144-145. Addison Wesley Longman Publishing Co., Inc., USA, 1998.
* Donald E. Knuth. The Art of Computer Programming, Volume 3 : (2nd Ed.) Sorting and
Searching, page 481-491. Addison Wesley Longman Publishing Co., Inc., USA, 1998.
* Xavier Leroy, Damien Doligez, Alain Frisch, Jacques Garrigue, Didier Rémy, and Jérôme
Vouillon. The ocaml system : Documentation and user's manual. INRIA, 3 :42.
* Chris Okasaki. Purely Functionnal Data Structures. Cambridge University Press, 1998.
page 17.
* Ronald Rivest. The md5 message-digest algorithm. Technical report, 1992.
* Jean Vuillemin. A data structure for manipulating priority queues.
21(4) :309-315, apr 1978.
  

