# Exercie1 : Résistance au stress thermique chez Caenorhabditis elegans

# ----------------------------------------------
# 1) chargement et description
c_elegans = read.table(file="c.elegans.txt",header=T)
dim(c_elegans)
View(c_elegans)
#[1] 800   4
plot(c_elegans)
par(mfrow=c(1,3))
hist(c_elegans$length)
barplot(table(c_elegans$temp))
barplot(table(c_elegans$num.tech))
## pour voir s'il y'a un effet technicien
boxplot(c_elegans$length~c_elegans$num.tech)
#Le boxplot ne montre pas de différence entre les 4 mesures.
# test de comparaison de model pour confirmer
model_complet = lm(length~.,data=c_elegans)
summary(model_complet)
sous_model = lm(length~.-num.tech,data=c_elegans)
summary(sous_model)
anova(sous_model,model_complet)
## la p-value montre qu'il n'y a pas d'effet technicien
boxplot(c_elegans$length~c_elegans$temp)
t.test(c_elegans$length~c_elegans$temp)
## donc il y'a une différence signicative entre les mesures à 20°C et celles à 37 °C

# 2) seule la variable longueur est aléatoire. 
hist(c_elegans$length)
## X20 longueur à 20°C et X37 la longueur à 37°C
par(mfrow=c(1,2))
hist(c_elegans$length[c_elegans$temp==20],xlim = c(500,1300))
hist(c_elegans$length[c_elegans$temp==37],xlim = c(500,1300))

## 
hist(c_elegans$length[c_elegans$temp==37],xlim = c(500,1300),freq = FALSE)
points(density(c_elegans$length[c_elegans$temp==37]),type="l",col="red",)
# pour 37°C nous deux groupes. Nous avons un modéle de mélange
# mclust pour estimer les paramètres d'un modèle de mélange
install.packages("mclust")
library(mclust)
res = mclustBIC(c_elegans$length[c_elegans$temp==37])
plot(res)

res1 = Mclust(c_elegans$length[c_elegans$temp==37],G=2)
plot(res1,what = "classification")
plot(res1,what = "BIC")
plot(res1,what = "uncertainty")
plot(res1,what = "density")

# classification
res1$classification
table(res1$classification)
# param
res1$parameters
## $pro les valeur pi1 et pi2
## [1] 0.5765429 0.4234571

## $mean moyennes associées
# 1         2 
# 755.9966 1004.6470 

#$variance
#$variance$modelName
#[1] "E"

#$variance$d
#[1] 1

#$variance$G
#[1] 2

#$variance$sigmasq
#[1] 4451.462  sigma1 et 2 égaux
res1$z  ## donne la proba d'appartenir à chaque classe

# ------------------------------------------------------------------------------
###    Exercice 2 : Simulations de données sous un modèle de mélange et inférence

# ---------------------------------------------

mu1 = 88
mu2 = 62
sigma1 = 7.5^2
sigma2 = 7.5^2
data = c(rnorm(500,mean=88,sd=7.5),rnorm(500,mean=62,sd=7.5))
hist(data)
data=c()
for (i in 1:100) {
  # indice de classe
  cl=rbinom(n=1,size=1,0.5)
  ## simuation en fonction de la classe
  if (cl==0) {
    data=c(data,rnorm(500,mean=88,sd=7.5))
    
  } else {
    data=c(data,rnorm(500,mean=62,sd=7.5))
  }
}
hist(data)

rbinom(n=1,size=1,0.5)
rmultinom(n=1,size=1,prob=c(1/3,1/3,1/3))
save(data, file="data_simul.RData")

# 2)
## init
load("data_simul.RData")
nbit <- 12
stock_value <- matrix(NA,ncol=7,nrow=nbit)
colnames(stock_value) <- c("pi1","pi2","m1","m2",
                           "sigma1","sigma2","loglik")
# initialisation aléatoire des paramètres du modèle
pi1 <- runif(1)
pi2 <- 1-pi1
mu1 <- runif(1,0,100)
mu2 <- runif(1,0,100)
sigma1 <- runif(1,0.5,10)
sigma2 <- runif(1,0.5,10)
get_loglike <- function(y){
  pi1*dnorm(y,mean=mu1,sd=sigma1)+pi2*dnorm(y,mean=mu2,sd=sigma2)
}
loglik <- sum(log(sapply(data_simul,get_loglike)))
loglik
stock_value[1,] <- c(pi1,pi2,mu1,mu2,sigma1,sigma2,loglik)
for(i in 2:nbit){
  # Etape E
  get_tij <- function(y){
    numt11 <- (pi1*dnorm(y,mean=mu1,sd=sigma1))
    numt12 <- (pi2*dnorm(y,mean=mu2,sd=sigma2))
    denum <- numt11+numt12
    t11 <- numt11/denum
    t12 <- numt12/denum
    return(c(t11,t12))
  }
  tij_etapec <- t(sapply(data_simul, get_tij))
  ## Etape M
  pi1 <- mean(tij_etapec[,1])
  pi2 <- mean(tij_etapec[,2])
  mu1 <- sum(data_simul*tij_etapec[,1])/sum(tij_etapec[,1])
  mu2 <- sum(data_simul*tij_etapec[,2])/sum(tij_etapec[,2])
  sigma1 <- sqrt(sum((data_simul-mu1)^2*tij_etapec[,1])/sum(tij_etapec[,1]))
  sigma2 <- sqrt(sum((data_simul-mu2)^2*tij_etapec[,2])/sum(tij_etapec[,2]))
  get_loglike <- function(y){
    pi1*dnorm(y,mean=mu1,sd=sigma1)+pi2*dnorm(y,mean=mu2,sd=sigma2)
  }
  loglik <- sum(log(sapply(data_simul,get_loglike)))
  stock_value[i,] <- c(pi1,pi2,mu1,mu2,sigma1,sigma2,loglik)
}
head(stock_value)

## Exercie3















