# Q1

data <- read.csv("velib5jours.txt", sep = " ")
summary(data)

# Fonction pour calculer la proportion de valeurs manquantes par variable
proportion_valeurs_manquantes <- function(data) {
  # Calcul du nombre de valeurs manquantes par colonne
  nb_valeurs_manquantes <- sapply(data, function(x) sum(is.na(x)))
  
  # Calcul de la proportion de valeurs manquantes
  proportion_manquantes <- nb_valeurs_manquantes / nrow(data)
  
  # Création d'un dataframe pour le résultat
  resultat <- data.frame(Nombre = nb_valeurs_manquantes, Proportion = proportion_manquantes)
  
  return(resultat)
}

# Utilisation de la fonction avec votre base de données
resultat <- proportion_valeurs_manquantes(data)

# Affichage du résultat
resultat



# Créer un histogramme pour chaque variable quantitative
for (var in names(data)[vars_quantitatives]) {
  print(ggplot(data, aes_string(x = var)) +
          geom_histogram(bins = 30, fill = "blue", color = "black") +
          theme_minimal() +
          labs(title = paste("Histogramme de", var), x = var, y = "Fréquence"))
}


barplot(table(data$noms))

library(Factoshiny)

res <- Factoshiny(data)



# Charger les packages
library(ggplot2)
library(FactoMineR)
library(factoextra)
# Identifier les colonnes quantitatives
vars_quantitatives <- sapply(data, is.numeric)
donnees_quantitatives <- data[, vars_quantitatives]

par(mar = c(4.1, 4.1, 1.1, 2.1))
res.PCA<-PCA(data,quali.sup=c(121),graph=FALSE)
plot.PCA(res.PCA,select='contrib  1213',habillage='contrib',title="Graphe des individus de l'ACP",cex=0.85,cex.main=0.85,cex.axis=0.85)

donnees_centrees_reduites <- scale(donnees_quantitatives,center = TRUE,scale=TRUE)

res_acp <- PCA(donnees_centrees_reduites, graph = F)

res_acp
# Créer le graphique du cercle de corrélation
fviz_pca_var(res_acp)



