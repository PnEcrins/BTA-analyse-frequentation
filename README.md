# BTA-analyse-frequentation

Scripts R de caractérisation standardisée comparative de la fréquentation d'espaces protégés.

Ils permettent de faire une analyse et une synthèse (sous forme de page web HTML) de la fréquentation dans un ou plusieurs espaces protégés, à partir de différentes sources de données (éco-compteurs, Outdoorvision, Strava, iNaturalist, itinéraires de randonnées).

Exemple du résultat sur les 13 espaces protégés du projet européen BiodivTourAlps (BTA) : https://pnecrins.github.io/BTA-analyse-frequentation/2_Outputs/Rapports_parcs/Rapport_parc_PNEcrins_V6.html

## Comment utiliser les scripts pour un nouvel espace protégé ?

Ce script a été développé dans l’objectif précis d'analyser sur les 13 parcs sélectionnés. Il est possible d’adapter le script pour l’appliquer à d’autres territoires, à condition d’être à l’aise avec R pour aller modifier le contenu des fonctions là où cela est nécessaire.

Pour lancer une analyse sur un nouveau territoire, il faudra s’assurer d’avoir les données suffisantes et de les placer dans les dossiers correspondants dans le dossier 0_Data :

- les données Strava obtenues par Strava Métro (obligatoires)
- les limites de l’espace protégé (obligatoires)
- les données éco-compteurs (fortement recommandées ; l’analyse peut fonctionner sans éco-compteur pour votre espace protégé – en fixant le paramètre `Run_EcoTempo` à `F` – mais il est nécessaire alors d’avoir les données des autres sites protégés pour que la corrélation entre éco-compteur et les données Strava / Outdoorvision / iNaturalist puisse être calculée).
- les données Outdoorvision (facultatives ; adapter le paramètre `Run_Outdoor` pour choisir si l’analyse doit inclure ces données ou non)
- les données Sentiers (facultatives ; adapter le paramètre `Run_Sentiers` pour choisir si l’analyse doit inclure ces données ou non).

Il faudra ensuite modifier l’object `parclist` du script principal, ainsi que les chemins d’accès dans les différentes fonctions du code pour s’assurer que chaque fichier puisse être chargé (par exemple dans la fonction `BTA_ChargerStrava` pour les données Strava). Il faudra pour cela avoir vérifié que le format des données correspond bien aux données utilisées dans cette analyse (voir section "0_Data" de ce document).

Le script qui crée le rapport automatisé est pensé pour analyser plusieurs parcs et les comparer. Si vous souhaitez créer un rendu similaire pour un seul espace protégé, il vous faudra alors adapter le script pour enlever toute la partie comparative des scripts d’analyses, mais également des fichiers .rmd qui codent le rapport. 

Questions à adresser à [https://conservara.fr/](conservara.fr).

## Structure des dossiers

### Contenu du dossier "0_Data"

- Ecocompteurs : Données de comptage des éco-compteurs. Chaque espace protégé doit avoir un dossier à son nom (avec le nom correspondant au nom utilisé dans la liste `parclist` dans les scripts). Le ou les fichiers doivent être au format csv et contenir 3 colonnes : `compteur` (nom de l’éco-compteur), `date` (date et heure de comptage, le format doit être strictement respecté, voir par exemple `PNV/PNV_Barmettes_Pralo_2009-2025.csv`), `donnees` (comptage). Le dossier contient également le fichier `BTA - données partenaires _ dati partner.xlsx` qui contient les coordonnées des éco-compteurs de chaque parc (une feuille par parc).
- Limites_shp : limites géographiques de chaque espace protégé au format shapefile ou geopackage. Certains des fichiers incluent plus que l’aire à cartographier, la sélection est alors faite dans le script `1.Script_Prepare_Data.R`, section « Limites du parc ».
- Outdoorvision : données flux Outdoorvision (obtenue par le PNE). Ces fichiers sont sourcés depuis le script `1.Script_Prepare_Data.R`, section « Outdoorvision ». Ces données sont optionnelles (le paramètre `Run_Outdoor` doit être fixé à `F` si ces données ne sont pas présentes).
- Sentiers : cartographie des itinéraires de randonnée de chaque espace protégé au format shapefile ou geopackage. Les fichiers doivent inclure une colonne `name` avec le nom du sentier. Ces données sont optionnelles (le paramètre Run_Sentiers doit être fixé à ‘F’ si ces données ne sont pas présentes).
- Strava : données flux Strava (obtenue par le PNE). Ces fichiers sont sourcés depuis le script `1.Script_Prepare_Data.R`, section "Strava" puis traités dans le script.

### Contenu du dossier "1_Scripts" :

- `00_Script_control_BTA_interparc.R` : principal script à partir duquel tous les autres scripts sont appelés. Il permet l’exécution de chaque étape de l’analyse : préparation des données des 13 parcs ; calcul du poids à accorder à chacun des jeux de données externe (Strava, Outdoorvision, iNaturalist en fonction des corrélations) ; analyse des données des 13 parcs ; comparaison de la fréquentation des 13 parcs ; création des rapports pour les 13 parcs.
- `01.Script_Prepare_Data.R` : script de préparation des données de chaque parc (limites du parc, formatage des données Outdoorvision et Strava, téléchargement des données iNaturalist, etc).
- `02.Script_Analyse_Data.R` : script d’analyse de la fréquentation standardisée du parc (par raster et sentiers de randonnées) et de la dynamique temporelle.
- `03.Script_Comparison.R` : script comparant les fréquentations des 13 espaces protégés.
- `BTA2 fonctions interparc.R` : script incluant des fonctions utilisées dans les 4 scripts mentionnés ci-dessus. Le but de ces fonctions est de rendre les scripts plus lisibles et flexibles.
- `BTA2_CreateMarkDownReport.rmd` : script qui permet la création du rapport automatisé (appelé également depuis `00_Script_control_BTA_interparc.R`). Ce script contient la majeure partie du texte du rapport, et appelle d’autres scripts (`BTA2_CreateMarkDownReport_`*) pour des ajouts spécifiques qui ne concernent pas l’ensemble des rapports (par exemple le script `BTA2_CreateMarkDownReport_outdoorPres.rmd` ajoute un contenu spécifique aux espaces protégés français pour lesquels les données Outdoorvision ont été fournies).
- logo : dossier contenant les logos à intégrer dans la colonne gauche des rapports automatisés.

### Contenu du dossier "2_Outputs" :

Le contenu de ce dossier est créé automatiquement par les scripts de l’analyse. Il contient :

- 0.iNaturalist_ready : données iNaturalist téléchargées et formatées dans le script `01.Script_Prepare_Data.R` (si le paramètre `Run_Download_iNat` est fixé à `T` dans le script principal)
- 0.Strava_ready : données Strava formatées par la fonction `BTA_PrepareStrava()` si le paramètre `Run_Prepare_Strava` est fixé à `T` dans le script principal.
- 1.Compiled_data & 2.Analysed_data : des fichiers sont sauvegardés aux étapes de l’analyse, elles sont stockées dans ce dossier.
- Outputs_spatiaux : contient les résultats de la cartographie de fréquentation standardisée au format .tif pour le raster de 500x500m et au format .gpkg pour les sentiers de randonnée. Ces produits ont été fournis aux parcs pour pouvoir être réutilisés.
- Rapports_parcs : contient les rapports finaux compilés par à la fin du script `00_Script_control_BTA_interparc.R`

------------------------------------------------

Cet outil a été piloté par le Parc national des Écrins et financé dans le cadre du projet BiodivTourAlps ALCOTRA n°20140.

<img width="1515" height="330" alt="BiodivTourAlps_logo_def" src="https://github.com/user-attachments/assets/ffaed87f-e11a-4a5e-a286-c9b002b61f51" />
