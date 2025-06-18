
#  Introduction à l’analyse de données avec R

Ce projet est un support de formation à l’introduction à l’analyse de données avec **R**.  
Il est destiné aux débutants souhaitant se familiariser avec les bases de la manipulation de données, de la visualisation et des statistiques descriptives dans R.

##  Structure du projet

```
intro-analyse-donnees-R/
├── README.md              # Présentation du projet
├── analyse_intro.Rmd      # Fichier RMarkdown principal
├── analyse_intro.html     # Rapport généré (optionnel)
├── data/                  # Données brutes ou nettoyées
│   └── donnees.csv
├── plots/                 # Graphiques produits
│   └── histogramme_age.png
├── scripts/               # Scripts R complémentaires
│   └── nettoyage.R
```

## Objectifs pédagogiques

- Importer et visualiser des données dans R
- Nettoyer et transformer des jeux de données
- Réaliser des analyses statistiques simples
- Créer des graphiques clairs avec `ggplot2`
- Générer un rapport dynamique avec `rmarkdown`

##  Exécution du projet

1. Ouvrir `analyse_intro.Rmd` avec RStudio
2. Installer les packages nécessaires :
   ```r
   install.packages(c("tidyverse", "ggplot2", "readr", "dplyr", "rmarkdown"))
   ```
3. Tricoter le fichier (`Knit`) pour obtenir un rapport HTML

##  Données utilisées

Les données utilisées dans ce projet sont fictives ou publiques et stockées dans le dossier `data/`. Elles comprennent des variables telles que l’âge, le sexe, le niveau d’étude, etc.

## Résultats

Quelques graphiques produits :

![exemple de graphique](plots/histogramme_age.png)

## Packages R utilisés

- `tidyverse`
- `dplyr`
- `ggplot2`
- `readr`
- `rmarkdown`

## Contact

Auteur : **Elisée AMADE**  
Mail : [aelisee09@gmail.com](mailto:aelisee09@gmail.com)  
LinkedIn : [linkedin.com/in/elisC3A9e-amade](https://www.linkedin.com/in/elisC3A9e-amade)

---

> Ce projet peut être librement réutilisé à des fins pédagogiques.
