library(FactoMineR)
library(ggplot2)
library(factoextra)

data <- read.csv(file = "Metadata-echantillons.csv", header = T, sep = ";")

## Analyse données manques

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

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("BiocVersion")
library(BiocManager)
library(BiocVersion)
install.packages("LEA")
library(LEA)
#devtools::
install.packages("mvtnorm")  
library(mvtnorm)
sigmaA=matrix(c(2,-1,-1,2),ncol=2)
data <- rmvnorm(100,mean = c(15,15),sigma=sigmaA)
plot(data[,1],data[,2],ylab="X",xlab="Y",
     main="Données simulées sous une loi normale bivariée",
     ylim=c(0,20),xlim=c(0,20))
abline(v=15)
abline(h=15)  
eigen(sigmaA)

## Un exemple sur des données réelles
install.packages("bnlearn")
library(bnlearn) ## pour avoir les données Marks
# Marks data from Mardia, 1974.
data(marks)
str(marks)
dim(marks)
plot(marks)

## analyse des correlation
library(corrplot)
## corrplot 0.92 loaded
corrplot(cor(marks))

## on fait la PCA avec FactoMineR
library(FactoMineR)
## etape 0 : on centre et on réduit les données
X <- scale(marks,center = TRUE,scale = TRUE)
colMeans(X)
## MECH VECT ALG ANL STAT
## 8.015211e-17 -1.945256e-16 5.653622e-17 1.252943e-16 1.162265e-16
apply(X,2,var)
## MECH VECT ALG ANL STAT
## 1 1 1 1 1
## on applique la fonction PCA
resPCA <- PCA(X,scale.unit = FALSE)

## on utilise le package factoextra
## pour avoir une représentation visuelle
library(factoextra)
## Loading required package: ggplot2
## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa
fviz_pca_biplot(resPCA)


library(datasets)
data(faithful)
par(mfrow=c(1,1))
hist(faithful$waiting, main="données faithful",
     freq=FALSE,
     xlab="temps d'attente entre deux éruptions en mins")
points(density(faithful$waiting),type="l")

install.packages("mclust")
library(mclust)
## Package ’mclust’ version 6.0.1
## Type ’citation("mclust")’ for citing this R package in publications.
res=Mclust(faithful$waiting)
par(mfrow=c(1,2))
res$parameters
plot(res,what = "density")
