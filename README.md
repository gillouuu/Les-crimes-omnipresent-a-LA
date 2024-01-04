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
1) Cloner le lien du repository :
2) Télécharger la base de données en cliquant sur ce [lien]( https://www.kaggle.com/datasets/bayusuarsa/crime-data-from-2020-to-present)
3) Ouvrir le projet avec l'application Rstudio
4) Ouvrir le fichier packages.R
5) Installer tous les packages présents dans les library (avec install.packages"*nom_du_packages*)
6) Sélectionner tous les install.packages puis cliquer sur run pour installer tous les packages
7) Supprimer toutes les lignes install.packages
8) Sélectionner toutes les library pui éxécuter en cliquant sur run
10) Ouvrir le dossier global.R , remplacer le chemin vers la base de données par votre propre chemin
11) Sélectionner l'ensemble des lignes puis cliquer sur run
12) Ouvrir le fichier app.R sélectionner l'ensemble des lignes puis cliquer sur runApp pour lancer l'application


### Structure de l'application 
* La partie ui correspond à l'interface utilisateur, à savoir ce que l'utilisateur verra
  * La partie sidebarPanel correspond à la partie de l'interface qui est en interaction avec l'utilisateur
  * La partie mainPanel renvoie les résultats en fonction des choix de l'utilisateur
* La partie server correspond à ce qui se passe quand l'utilisateur fais ces choix et n'apparait pas sur l'application


### Exemple d'utilisation 

![ex_utilisation](https://github.com/gillouuu/Les-crimes-omnipresent-a-LA/assets/152622879/ea8cc58c-2364-4408-84ce-9e29b25a16a7)


#### Contact 
Pour de plus amples informations n'hésitez pas à me contacter 

Par mail : corentingilles81@gmail.com

Via [LinkedIn](https://www.linkedin.com/in/corentin-gilles-bb25961b7/)



