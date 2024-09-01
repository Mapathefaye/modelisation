
# Installation des packages si nécessaire
install.packages("mclust")
install.packages("cluster")

# Charger les packages
library(mclust)
library(cluster)

# Chargement des données
velib_data <- read.table("velib5jours.txt", sep = " ")

# Prétraitement des données si nécessaire

# Choix du nombre de clusters

# Entraînement du modèle de mélange gaussien avec le nombre de clusters choisi
best_model <- Mclust(velib_data, G = 2)

# Attribution des stations vélib aux clusters
cluster_assignments <- best_model$classification
cluster_assignments
# Analyse des résultats
table(cluster_assignments)
plot(cluster_assignments,what = "classification")
plot(cluster_assignments,what = "BIC")
