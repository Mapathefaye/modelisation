## ------------------------------
data=read.table("PaintedTurtles.txt",header = T)
dim(data) ## 48 individus stats (des tortues)
## 4 variables


## ----out.width='2.5in'---------
table(data$sex)
par(mfrow=c(2,2))
barplot(table(data$sex))
hist(data$length)
hist(data$width)
hist(data$height)


## ----out.width='2.5in'---------
library(FactoMineR)
resPCA <- PCA(data,scale.unit = TRUE,quali.sup = 1,graph = FALSE)
## la variable sexe n'est pas incluse dans l'ACP
## on spécifie qu'il s'agit d'une variable supplémentaire
library(factoextra,verbose = FALSE)
fviz_pca_biplot(resPCA,col.ind = data$sex)


## ----out.width='.6\\linewidth', fig.align='center',messages=FALSE,eval=FALSE----
## load("data/Msig3transp.RData")


## ----out.width='2.5in'---------
load("data/Msig3transp.RData")
dim(Msig3transp) ## 30 lignes, 156 colonnes 
rownames(Msig3transp)
round(Msig3transp,2)[1:5, 1:6]


## ------------------------------
## la fonction strsplit coupe une chaine de caractère 
strsplit(rownames(Msig3transp)[1],"_")
## la fonction apply permet d'appliquer 
## une opération à chaque élément d'une liste
## la fonction sapply permet de "simplifier"
## (transformer la sortie de apply qui est une liste)
## en un simple vecteur
info1 <- sapply(1:30, function(x) strsplit(rownames(Msig3transp)[x],"_")[[1]][1])
info2 <- sapply(1:30, function(x) strsplit(rownames(Msig3transp)[x],"_")[[1]][2])
info3 <- sapply(1:30, function(x) strsplit(rownames(Msig3transp)[x],"_")[[1]][3])
table(info1) ## contient une info que l'on devine etre l'indice de l'individu
table(info2) ## contient une info que l'on devine être le type cellulaire
table(info3) ## info difficile à décrypter ... batch ? labo ? 


## ----out.width='2.5in'---------
par(mfrow=c(2,2))
hist(Msig3transp[,17])
hist(Msig3transp[,109])
hist(Msig3transp[,7])
hist(Msig3transp[,78])
range(Msig3transp)
par(mfrow=c(1,1))
hist(as.matrix(Msig3transp))


## ----out.width='2.5in'---------
library(FactoMineR)
info1 <- factor(info1);info2 <- factor(info2)
info3 <- factor(info3)
resPCA <- PCA(Msig3transp,scale.unit = TRUE,graph=FALSE)
barplot(resPCA$eig[,2],main="% de variance expliquée en fonction de la composante")
## on garde les 2 premières composantes
par(mfrow=c(1,2))
plot(resPCA,choix = "ind")
plot(resPCA,choix = "var")
library(factoextra,verbose = FALSE)
par(mfrow=c(1,2))
fviz_pca_biplot(resPCA,habillage = info1,invisible = "var")
fviz_pca_biplot(resPCA,habillage = info2,invisible = "var")
fviz_pca_biplot(resPCA,habillage = info3,invisible = "var")


## ------------------------------
table(info2,info1)


## ----eval=TRUE,echo=TRUE-------
monurl <- "https://bowtie-bio.sourceforge.net/recount/countTables/bottomly_count_table.txt"
data <- read.table(monurl,
                   header=TRUE,row.names = 1)
dim(data)
monurl2 <- "https://bowtie-bio.sourceforge.net/recount/phenotypeTables/bottomly_phenodata.txt"
phenotype <- read.csv2(monurl2,sep=" ")
phenotype
dim(phenotype)
table(phenotype$strain)
table(phenotype$experiment.number)
table(phenotype$lane.number)


## ----out.width='2.5in'---------
data <- as.matrix(data)
par(mfrow=c(2,2))
hist(data[17,])
hist(data[109,])
hist(data[7,])
hist(data[78,])
range(data)
par(mfrow=c(1,1))
hist(data)
table(data==0)


## ------------------------------
table(rowMeans(data)==0) ## uniquement 13932 variables non nulles
## on log les données
logdata <- log(data[rowMeans(data)!=0,]+0.1)
par(mfrow=c(2,2))
hist(logdata[17,])
hist(logdata[109,])
hist(logdata[7,])
hist(logdata[78,])
range(logdata)
par(mfrow=c(1,1))
hist(logdata)


## ----out.width='2.5in'---------
library(FactoMineR)
resPCA <- PCA(t(logdata),scale.unit = TRUE,graph=FALSE)
barplot(resPCA$eig[,2],main="% de variance expliquée en fonction de la composante")
## on garde les 2 premières composantes
par(mfrow=c(1,2))
plot(resPCA,choix = "ind")
#plot(resPCA,choix = "var") graphique des variable peu lisible...
library(factoextra,verbose = FALSE)
par(mfrow=c(1,2))
lignee <- factor(phenotype$strain)
lane.number <- factor(phenotype$lane.number)
exp.number <- factor(phenotype$experiment.number)
fviz_pca_biplot(resPCA,habillage = lignee, invisible = "var")
fviz_pca_biplot(resPCA,habillage = lane.number,invisible = "var")
fviz_pca_biplot(resPCA,habillage = exp.number,invisible = "var")


## ----eval=FALSE----------------
## if (!require("BiocManager", quietly = TRUE))
##     install.packages("BiocManager")
## BiocManager::install("phyloseq")


## ----eval=TRUE-----------------
library(phyloseq)
data("GlobalPatterns", package = "phyloseq")
GPOTUs = as.matrix(t(phyloseq::otu_table(GlobalPatterns)))
#GPOTUs[1:4, 6:13]
#help(GlobalPatterns)


## ------------------------------
dim(GPOTUs)
str(GPOTUs)
GPOTUs[1:4, 6:13]
rownames(GPOTUs)
min(GPOTUs);max(GPOTUs)
table(colSums(GPOTUs)==0)


## ------------------------------
logGPOTUs <- log(GPOTUs+1)


## ----out.width='2.5in'---------
## on a bcp de variables, on travaille uniquement 
## a partir des bactéries les plus "variables"
myvar <- apply(GPOTUs,2,var)
resPCA <- PCA(log(GPOTUs[,order(myvar,decreasing = TRUE)[1:400]]+1),graph=FALSE)
par(mfrow=c(1,1))
fviz_pca_ind(resPCA)

