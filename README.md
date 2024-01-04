# Etude des crimes à Los Angeles
## A propos du projet

Comme vous le savez sûrement, Los Angeles est une ville aux Etats-Unis avec un taux de criminalité très important. Grâce à un jeu de données très complet nous disposons de l'ensemble des crimes réalisés à LA sur plusieurs années.
Cependant, cette base de données étant immense et très complexe, il est difficile de se faire une visualisation de tout cela.
Afin de répondre à cette problématique, j'ai décidé de faire un projet dans lequel l'utilisateur pourrait se concentrer sur la victime afin de découvrir les différents crimes commis.

### Pour commencer 
#### Les prérequis 

    R (version 3.6.0 minimum)
    RStudio
    Shiny package

#### Installation
1) cloner le lien du repository :
2) télécharger la base de données en cliquant sur ce [lien]( https://www.kaggle.com/datasets/bayusuarsa/crime-data-from-2020-to-present)
3) ouvrir le projet avec l'application Rstudio
4) ouvrir le fichier packages.R
5) installer tous les packages présents dans les library (avec install.packages"*nom_du_packages*)
6) sélectionner tous les install.packages puis cliquer sur run pour installer tous les packages
7) supprimer toutes les lignes install.packages
8) sélectionner toutes les library pui éxécuter en cliquant sur run
9) ouvrir le dossier global.R , remplacer le chemin vers la base de données par votre propre chemin
10) sélectionner l'ensemble des lignes puis cliquer sur run
11) ouvrir le fichier app.R sélectionner l'ensemble des lignes puis cliquer sur runApp pour lancer l'application


### Structure de l'application 
* La partie ui correspond à l'interface utilisateur, à savoir ce que l'utilisateur verra
  * La partie sidebarPanel correspond à la partie de l'interface qui est en interaction avec l'utilisateur
  * La partie mainPanel renvoie les résultats en fonction des choix de l'utilisateur
* La partie server correspond à ce qui se passe quand l'utilisateur fais ces choix et n'apparait pas sur l'application





