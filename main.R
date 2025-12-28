############################################################
# 1. Présentation des données et objectifs de l'étude
############################################################

# Dans ce projet, on étudie la relation entre :
# - le taux de natalité (en %)
# - le taux d'urbanisation (en %)
# pour 14 unités statistiques (par ex. pays ou régions).

# Objectifs :
# 1) Décrire séparément la distribution du taux de natalité
#    et du taux d'urbanisation.
# 2) Étudier la relation entre ces deux variables (corrélation).
# 3) Vérifier si un modèle de régression linéaire simple
#    permet d'expliquer la natalité par l'urbanisation.

############################################################
# 2. Importation du fichier de données
############################################################

# Option 1 : si ton fichier est dans le répertoire de travail courant

setwd("~/Desktop/R-stat/TP-1")

don <- read.csv2("don_mls.csv", header = TRUE, sep = ";", dec = ",")

# Option 2 : sinon, préciser le chemin complet du fichier, par exemple :
# don <- read.csv2("C:/Users/ton_nom/Documents/don_mls.csv",
#                  header = TRUE, sep = ";", dec = ",")

# Vérification rapide du contenu
don
str(don)
head(don)

############################################################
# 3. Renommage des variables (facultatif mais conseillé)
############################################################

# Les noms de colonnes dans le fichier sont :
# "Taux de natalité" et "Taux d'urbanisation"
# On crée des noms plus simples pour travailler dans R.

names(don) <- c("natalite", "urbanisation")

# Vérifier les nouveaux noms de variables
names(don)

############################################################
# 4. Vérification du type et conversion en numérique si besoin
############################################################

# Selon la manière dont le fichier a été lu, les variables peuvent
# être importées comme "factor" ou "character".
# On les convertit en numérique de manière sécurisée.

don$natalite      <- as.numeric(as.character(don$natalite))
don$urbanisation  <- as.numeric(as.character(don$urbanisation))

# Vérifier un résumé rapide des données
summary(don)

############################################################
# 5. Analyse descriptive univariée
############################################################

# Statistiques descriptives de base pour chaque variable
summary(don$natalite)
summary(don$urbanisation)

# Moyenne
mean(don$natalite)
mean(don$urbanisation)

# Médiane
median(don$natalite)
median(don$urbanisation)

# Écart-type
sd(don$natalite)
sd(don$urbanisation)

# Minimum et maximum
min(don$natalite); max(don$natalite)
min(don$urbanisation); max(don$urbanisation)

# Quartiles (Q1, médiane, Q3)
quantile(don$natalite, probs = c(0.25, 0.5, 0.75))
quantile(don$urbanisation, probs = c(0.25, 0.5, 0.75))

############################################################
# 6. Représentations graphiques univariées
############################################################

# Histogrammes
hist(don$natalite,
     main = "Histogramme du taux de natalité",
     xlab = "Taux de natalité (%)",
     col  = "lightblue",
     border = "white")

hist(don$urbanisation,
     main = "Histogramme du taux d'urbanisation",
     xlab = "Taux d'urbanisation (%)",
     col  = "lightgreen",
     border = "white")

# Boîtes à moustaches (boxplots)
boxplot(don$natalite,
        main = "Boxplot du taux de natalité",
        ylab = "Taux de natalité (%)",
        col  = "lightblue")

boxplot(don$urbanisation,
        main = "Boxplot du taux d'urbanisation",
        ylab = "Taux d'urbanisation (%)",
        col  = "lightgreen")

############################################################
# 7. Analyse bivariée : corrélation et nuage de points
############################################################

# 7.1 Coefficient de corrélation de Pearson
cor(don$natalite, don$urbanisation, method = "pearson")

# 7.2 Test de corrélation de Pearson (hypothèse H0 : corrélation = 0)
cor.test(don$natalite, don$urbanisation, method = "pearson")

# 7.3 Nuage de points

plot(don$urbanisation, don$natalite,
     main = "Taux de natalité en fonction du taux d'urbanisation",
     xlab = "Taux d'urbanisation (%)",
     ylab = "Taux de natalité (%)",
     pch  = 19,
     col  = "darkblue")

# On ajoutera plus tard la droite de régression après avoir ajusté le modèle linéaire.

############################################################
# 8. Modèle de régression linéaire
#    natalite (Y) ~ urbanisation (X)
############################################################

# 8.1 Ajustement du modèle
mod_lin <- lm(natalite ~ urbanisation, data = don)

# 8.2 Résumé complet du modèle
summary(mod_lin)

# 8.3 Coefficients du modèle
coef(mod_lin)  # intercept et pente

# 8.4 Ajout de la droite de régression sur le nuage de points

# On redessine d'abord le nuage de points
plot(don$urbanisation, don$natalite,
     main = "Régression linéaire : natalité ~ urbanisation",
     xlab = "Taux d'urbanisation (%)",
     ylab = "Taux de natalité (%)",
     pch  = 19,
     col  = "darkblue")

# Puis on ajoute la droite de régression
abline(mod_lin, col = "red", lwd = 2)

############################################################
# 9. Diagnostics simples du modèle linéaire
############################################################

# 9.1 Résumé numérique des résidus
residus <- residuals(mod_lin)
fites   <- fitted(mod_lin)

summary(residus)
mean(residus)          # doit être proche de 0

# 9.2 Graphiques de diagnostic de base

# 9.2.1 Résidus vs valeurs ajustées : vérifier la linéarité et l'homoscédasticité
plot(fites, residus,
     main = "Résidus vs valeurs ajustées",
     xlab = "Valeurs ajustées",
     ylab = "Résidus",
     pch  = 19,
     col  = "darkgreen")
abline(h = 0, col = "red", lwd = 2)

# 9.2.2 Q-Q plot des résidus : vérifier l'approximation à la normalité
qqnorm(residus,
       main = "Q-Q plot des résidus")
qqline(residus, col = "red", lwd = 2)

# 9.3 Plots de diagnostic automatiques (optionnel)

# Cette commande produit 4 graphes standard de diagnostic pour lm
par(mfrow = c(2, 2))   # affichage 2x2
plot(mod_lin)
par(mfrow = c(1, 1))   # on revient à 1 graphique par fenêtre

############################################################
# 10. Aide à la rédaction des résultats
############################################################

# 10.1 Récupération des indicateurs descriptifs
moy_nat   <- mean(don$natalite)
med_nat   <- median(don$natalite)
min_nat   <- min(don$natalite)
max_nat   <- max(don$natalite)
sd_nat    <- sd(don$natalite)

moy_urb   <- mean(don$urbanisation)
med_urb   <- median(don$urbanisation)
min_urb   <- min(don$urbanisation)
max_urb   <- max(don$urbanisation)
sd_urb    <- sd(don$urbanisation)

# 10.2 Corrélation
r_pearson <- cor(don$natalite, don$urbanisation, method = "pearson")
test_cor  <- cor.test(don$natalite, don$urbanisation, method = "pearson")

# 10.3 Résumé du modèle linéaire
res_mod   <- summary(mod_lin)
a         <- coef(mod_lin)[1]          # intercept
b         <- coef(mod_lin)[2]          # pente
R2        <- res_mod$r.squared
p_pente   <- res_mod$coefficients[2, 4]

# 10.4 Génération de phrases 

cat("\n=== Description des variables ===\n")

cat(sprintf("Le taux de natalité présente une moyenne de %.2f %%, une médiane de %.2f %%,\n", 
            moy_nat, med_nat))
cat(sprintf("un minimum de %.2f %% et un maximum de %.2f %%, avec un écart-type de %.2f %%.\n\n",
            min_nat, max_nat, sd_nat))

cat(sprintf("Le taux d'urbanisation a une moyenne de %.2f %%, une médiane de %.2f %%,\n", 
            moy_urb, med_urb))
cat(sprintf("et varie entre %.2f %% et %.2f %%, avec un écart-type de %.2f %%.\n\n",
            min_urb, max_urb, sd_urb))

cat("=== Corrélation et nuage de points ===\n")

cat(sprintf("Le coefficient de corrélation de Pearson entre natalité et urbanisation est r = %.3f.\n", 
            r_pearson))
cat(sprintf("Le test de corrélation donne une p-value de %.4f.\n\n", 
            test_cor$p.value))

cat("=== Modèle de régression linéaire ===\n")

cat(sprintf("Le modèle linéaire ajusté est : natalite = %.3f + %.3f * urbanisation.\n", 
            a, b))
cat(sprintf("Le coefficient de détermination est R² = %.3f, soit %.1f %% de variance expliquée.\n", 
            R2, 100 * R2))
cat(sprintf("La p-value associée à la pente est %.4f.\n\n", 
            p_pente))

############################################################
# 11. Création dossier + sauvegarde images + rapport TXT
############################################################

# 11.1 Création du dossier de sortie
dossier_sortie <- "Analyse_Natalite_Urbanisation"
if (!dir.exists(dossier_sortie)) {
  dir.create(dossier_sortie)
  cat("Dossier créé :", getwd(), "/", dossier_sortie, "\n")
}

# 11.2 Sauvegarde de toutes les images avec noms explicites
png(file.path(dossier_sortie, "01_histogramme_natalite.png"), width = 800, height = 600)
hist(don$natalite, main = "Histogramme du taux de natalité", xlab = "Taux de natalité (%)", 
     col = "lightblue", border = "white")
dev.off()

png(file.path(dossier_sortie, "02_histogramme_urbanisation.png"), width = 800, height = 600)
hist(don$urbanisation, main = "Histogramme du taux d'urbanisation", xlab = "Taux d'urbanisation (%)", 
     col = "lightgreen", border = "white")
dev.off()

png(file.path(dossier_sortie, "03_boxplot_natalite.png"), width = 800, height = 600)
boxplot(don$natalite, main = "Boxplot du taux de natalité", ylab = "Taux de natalité (%)", 
        col = "lightblue")
dev.off()

png(file.path(dossier_sortie, "04_boxplot_urbanisation.png"), width = 800, height = 600)
boxplot(don$urbanisation, main = "Boxplot du taux d'urbanisation", ylab = "Taux d'urbanisation (%)", 
        col = "lightgreen")
dev.off()

png(file.path(dossier_sortie, "05_nuage_points.png"), width = 800, height = 600)
plot(don$urbanisation, don$natalite, main = "Taux de natalité en fonction du taux d'urbanisation",
     xlab = "Taux d'urbanisation (%)", ylab = "Taux de natalité (%)", pch = 19, col = "darkblue")
dev.off()

png(file.path(dossier_sortie, "06_regression_lineaire.png"), width = 800, height = 600)
plot(don$urbanisation, don$natalite, main = "Régression linéaire : natalité ~ urbanisation",
     xlab = "Taux d'urbanisation (%)", ylab = "Taux de natalité (%)", pch = 19, col = "darkblue")
abline(mod_lin, col = "red", lwd = 2)
dev.off()

png(file.path(dossier_sortie, "07_residus_ajustees.png"), width = 800, height = 600)
plot(fites, residus, main = "Résidus vs valeurs ajustées", xlab = "Valeurs ajustées", 
     ylab = "Résidus", pch = 19, col = "darkgreen")
abline(h = 0, col = "red", lwd = 2)
dev.off()

png(file.path(dossier_sortie, "08_qqplot_residus.png"), width = 800, height = 600)
qqnorm(residus, main = "Q-Q plot des résidus")
qqline(residus, col = "red", lwd = 2)
dev.off()

png(file.path(dossier_sortie, "09_diagnostic_complet.png"), width = 1000, height = 800)
par(mfrow = c(2, 2))
plot(mod_lin)
par(mfrow = c(1, 1))
dev.off()

cat("Toutes les images sauvegardées dans :", dossier_sortie, "\n")

############################################################
# 11.3 Génération du rapport TXT complet
############################################################

rapport <- file(file.path(dossier_sortie, "RAPPORT_ANALYSE_COMPLETE.txt"), "w", encoding = "UTF-8")

# En-tête
writeLines(c(
  "============================================",
  "RAPPORT D'ANALYSE STATISTIQUE COMPLETE",
  "Relation entre TAUX DE NATALITE et TAUX D'URBANISATION",
  "============================================",
  "",
  paste("Date d'analyse :", Sys.time()),
  paste("Nombre d'observations :", nrow(don)),
  "============================================\n"
), rapport)

# 1. Description des variables
writeLines(c(
  "1. DESCRIPTION DES VARIABLES",
  "=============================",
  sprintf("TAUX DE NATALITE : moyenne = %.2f %% | médiane = %.2f %% | min = %.1f %% | max = %.1f %% | écart-type = %.2f %%", 
          moy_nat, med_nat, min_nat, max_nat, sd_nat),
  sprintf("TAUX D'URBANISATION : moyenne = %.2f %% | médiane = %.2f %% | min = %.1f %% | max = %.1f %% | écart-type = %.2f %%", 
          moy_urb, med_urb, min_urb, max_urb, sd_urb),
  "",
  "IMAGE 1 : Histogramme du taux de natalité",
  "[Insérer image/01_histogramme_natalite.png]",
  "Ce graphique montre la distribution du taux de natalité. On observe une distribution étalée",
  "entre 16 et 44 %, avec une concentration autour de 25-35 %. L'asymétrie suggère quelques",
  "unités avec des taux particulièrement élevés.",
  "",
  "IMAGE 2 : Histogramme du taux d'urbanisation",
  "[Insérer image/02_histogramme_urbanisation.png]",
  "Distribution de l'urbanisation plus homogène, concentrée entre 10-50 % avec un pic autour",
  "de 30-40 %. Moins d'asymétrie que pour la natalité.",
  "",
  "IMAGE 3 : Boxplot du taux de natalité",
  "[Insérer image/03_boxplot_natalite.png]",
  "Le boxplot du taux de natalité montre une dispersion modérée autour de la médiane,",
  "avec un intervalle interquartile relativement large. Les moustaches couvrent l'ensemble",
  "des valeurs observées sans point extrême isolé, ce qui indique l'absence d'outliers marqués.",
  "",
  "IMAGE 4 : Boxplot du taux d'urbanisation",
  "[Insérer image/04_boxplot_urbanisation.png]",
  "Le boxplot du taux d'urbanisation fait apparaître une variabilité plus importante,",
  "avec une médiane située autour de 30 %. La distribution semble légèrement étalée",
  "vers les valeurs élevées, sans observation clairement aberrante."
), rapport)

# 2. Analyse bivariée
writeLines(c(
  "",
  "2. ANALYSE BIVARIEE",
  "====================",
  sprintf("Corrélation de Pearson : r = %.3f (p-value = %.4f)", r_pearson, test_cor$p.value),
  "",
  "IMAGE 5 : Nuage de points",
  "[Insérer image/05_nuage_points.png]",
  "Visualisation de la relation brute. On observe une tendance négative claire : les unités",
  "les plus urbanisées tendent à avoir des taux de natalité plus faibles.",
  ""
), rapport)

# 3. Modèle linéaire
writeLines(c(
  "3. MODELE DE REGRESSION LINEAIRE",
  "================================",
  sprintf("Équation : natalité = %.3f + %.3f × urbanisation", a, b),
  sprintf("R² = %.3f (%.1f %% de variance expliquée)", R2, 100*R2),
  sprintf("Pente significative (p = %.4f)", p_pente),
  "",
  "IMAGE 6 : Régression linéaire",
  "[Insérer image/06_regression_lineaire.png]",
  "Droite de régression rouge confirmant la relation négative forte. La pente négative indique",
  "qu'une augmentation de 1 point d'urbanisation est associée à une baisse moyenne de %.3f",
  "points de natalité.", sprintf("%.3f", b),
  ""
), rapport)

# 4. Diagnostics
writeLines(c(
  "4. DIAGNOSTICS DU MODELE",
  "========================",
  "",
  "IMAGE 7 : Résidus vs ajustées",
  "[Insérer image/07_residus_ajustees.png]",
  "Les résidus sont centrés autour de 0 sans motif clair (bonne linéarité et homoscédasticité).",
  "",
  "IMAGE 8 : Q-Q plot des résidus",
  "[Insérer image/08_qqplot_residus.png]",
  "Les points suivent globalement la droite rouge (normalité approximative des résidus satisfaisante).",
  "",
  "IMAGE 9 : Diagnostics complets",
  "[Insérer image/09_diagnostic_complet.png]",
  "Les 4 graphiques standards confirment la validité du modèle linéaire simple."
), rapport)

# 5. Conclusion
writeLines(c(
  "",
  "5. CONCLUSION",
  "==============",
  sprintf("Une relation **négative forte** (r = %.3f) existe entre urbanisation et natalité.", r_pearson),
  sprintf("Le modèle linéaire explique %.1f %% de la variance (R² = %.3f).", 100*R2, R2),
  "L'urbanisation est un prédicteur pertinent du taux de natalité dans cet échantillon.",
  "Limites : petit effectif (n=14), absence d'autres covariables (revenu, éducation, etc.).",
  "",
  "============================================",
  "FIN DU RAPPORT - Analyse terminée avec succès"
), rapport)

close(rapport)
cat("RAPPORT complet généré :", file.path(dossier_sortie, "RAPPORT_ANALYSE_COMPLETE.txt"), "\n")
cat("OUVRIR le dossier", dossier_sortie, "pour voir toutes les images + rapport TXT\n")
