# 📊 Analyse Statistique : Natalité et Urbanisation

Projet d'analyse de la relation entre le taux de natalité et le taux d'urbanisation à l'aide de R, incluant une régression linéaire simple et une application Shiny interactive.

## 🎯 Objectifs du Projet

Ce projet étudie la relation statistique entre deux variables démographiques :
- **Taux de natalité** (en %)
- **Taux d'urbanisation** (en %)

### Questions de recherche

1. Quelle est la distribution de chaque variable ?
2. Existe-t-il une corrélation entre natalité et urbanisation ?
3. Un modèle de régression linéaire peut-il expliquer cette relation ?

## 📁 Structure du Projet

.

├── don_mls.csv # Données brutes (n=14)

├── main.R # Script d'analyse principal

├── app.R # Application Shiny interactive

├── rapport-final.Rmd # Rapport R Markdown

├── rapport-final-v2.Rmd # Rapport R Markdown (version 2)

├── RAPPORT_ANALYSE_COMPLETE.txt # Rapport texte complet

└── images/ # Visualisations générées

├── 01_histogramme_natalite.jpg

├── 02_histogramme_urbanisation.jpg

├── 03_boxplot_natalite.jpg

├── 04_boxplot_urbanisation.jpg

├── 05_nuage_points.jpg

├── 06_regression_lineaire.jpg

├── 07_residus_ajustees.jpg

├── 08_qqplot_residus.jpg

└── 09_diagnostic_complet.jpg


## 🔧 Technologies Utilisées

- **R** (version ≥ 4.0)
- **Packages R** :
  - `shiny` - Application web interactive
  - `ggplot2` - Visualisation avancée
  - `dplyr` - Manipulation de données
  - `shinythemes` - Thèmes pour Shiny
  - `rmarkdown` - Génération de rapports

## 🚀 Installation et Utilisation

### Prérequis
install.packages(c("shiny", "ggplot2", "dplyr", "shinythemes", "rmarkdown"))

### Exécuter l'analyse principale
source("main.R")

### Lancer l'application Shiny
shiny::runApp("app.R")

### Générer le rapport HTML
rmarkdown::render("rapport-final-v2.Rmd")

## 📊 Résultats Clés

### Statistiques Descriptives
- **Natalité** : moyenne = 31.11%, médiane = 31.80%, écart-type = 10.00%
- **Urbanisation** : moyenne = 29.79%, médiane = 30.80%, écart-type = 15.57%

### Analyse de Corrélation
- **Coefficient de Pearson** : r = -0.621
- **p-value** : 0.0177
- **Conclusion** : Corrélation négative significative

### Modèle de Régression Linéaire
natalité = 42.991 - 0.399 × urbanisation


- **R²** : 0.386 (38.6% de variance expliquée)
- **Interprétation** : Une augmentation de 1% du taux d'urbanisation est associée à une baisse de 0.4% du taux de natalité

## 📈 Visualisations

### Régression Linéaire

![Régression Linéaire](06_regression_lineaire.jpg)

*La droite de régression montre clairement la relation négative entre urbanisation et natalité.*

### Diagnostics du Modèle

![Diagnostics](09_diagnostic_complet.jpg)

*Les graphiques de diagnostic valident les hypothèses du modèle linéaire.*

## 🌐 Application Shiny Interactive

L'application Shiny offre :
- **Filtrage dynamique** des données
- **Visualisations interactives** (histogrammes, boxplots, nuages de points)
- **Affichage du modèle** de régression en temps réel
- **Tests statistiques** (corrélation, coefficients)
- **Interface moderne** avec thème Bootstrap

### Onglets disponibles

1. 🎯 **Dashboard** : Vue d'ensemble avec tous les graphiques
2. 📈 **Statistiques** : Résumé descriptif et table de données
3. 📐 **Modèle** : Détails du modèle de régression
4. 🔗 **Corrélation** : Test de Pearson et interprétation

## 📝 Méthodologie

### 1. Analyse Descriptive Univariée
- Calcul des statistiques de tendance centrale et de dispersion
- Visualisation des distributions (histogrammes, boxplots)

### 2. Analyse Bivariée
- Test de corrélation de Pearson
- Nuage de points pour visualiser la relation

### 3. Régression Linéaire Simple
- Ajustement du modèle : `lm(natalite ~ urbanisation)`
- Évaluation de la qualité d'ajustement (R², p-values)
- Diagnostics de régression (résidus, normalité, homoscédasticité)

## ⚠️ Limites de l'Étude
- **Petit échantillon** : n=14 observations
- **Variables omises** : revenu, éducation, politiques publiques non considérées
- **Causalité** : la corrélation n'implique pas de relation causale
- **Modèle simple** : régression linéaire uniquement (pas de termes quadratiques ou interactions)

## 📖 Références

- Données synthétiques pour illustration pédagogique
- Méthodes statistiques standard (régression OLS)

## 👤 Auteur

**Mazzez Mohamed Amine**
-  Étudiant ingénieur 
- 📍 Ariana, Tunisie

---

*Projet réalisé dans le cadre d'une analyse statistique en R pour un projet de classe - Décembre 2025*
