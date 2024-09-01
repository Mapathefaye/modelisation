## Exercice1 : Tortue
# ------------------------------------------------------------------------------
# a)
tortues = read.table(file="PaintedTurtles.txt",header = T)
summary(tortues)
dim(tortues) # 48 rows and 4 col
# b)

par(mfrow=c(2,2))
barplot(table(tortues$sex))
hist(tortues$length)
hist(tortues$width)
hist(tortues$height)

#Une variable qualitative (sexe : F ou H), que l’on peut modéliser par une loi de Bernoulli.
#Trois variables quantitatives, modélisables par des lois normales (la variable height est ce-
# pendant très asymétrique

# c) c) Réaliser une ACP et interpréter les résultats de cette visualisation.
install.packages("FactoMineR")
library(FactoMineR)
install.packages("factoextra")
install.packages('ggplot2')
library(ggplot2)
library(factoextra)
resPCA = PCA(tortues,scale.unit=TRUE,quali.sup=1,graph=FALSE)
install.packages(c("Factoshiny","missMDA","FactoInvestigate"))
library(Factoshiny)
library(missMDA)
library(FactoInvestigate)
library(factoextra,verbose = FALSE)
## Loading required package: ggplot2
## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa
fviz_pca_biplot(resPCA,col.ind = tortues$sex)
