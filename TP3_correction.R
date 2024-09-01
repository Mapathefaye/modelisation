
## pour la loi de Dirichlet
library(LaplacesDemon)


## modele M1 sans admixture

n=12 # nb indiv
K=2 # nb population 
L=5 #nb locus
pi=c(3/4,1/4) # proportion de chaque pop
# proportion des allèles 
# pour chaque locus
# pour la pop=1
G1=cbind(c(1/3,1/3,1/3),
         c(4/6,1/6,1/6),
         c(3/16,7/16,6/16),
         c(4/10,1/10,5/10),
         c(49/60,10/60,1/60))
# proportion des allèles 
# pour chaque locus
# pour la pop=2
G2=cbind(c(3/16,7/16,6/16),
         c(1/3,1/3,1/3),
         c(4/10,1/10,5/10),
         c(49/60,10/60,1/60),
         c(4/6,1/6,1/6))


# on tire le num de pop pour chaque indiv
z=rmultinom(n,size = 1,prob = pi) 

# on récupère les indices de pop
pop=apply(z,2,which.max) 

# matrice Y contenant les info alléliques
# pour chaque individu et chaque locus
Y <- matrix(ncol=L,nrow=n)
# on génére les données sous le modèle M1
for(i in 1:n){
  if(pop[i]==1){
    for(l in 1:L){
      Y[i,l] <- which.max(rmultinom(n=1,size=1,prob = G1[,l]))
    }} else {
      for(l in 1:L){
        Y[i,l] <- which.max(rmultinom(n=1,size=1,prob = G2[,l]))
      }
    }
}

# matrice contenant les infos alléliques simulées
Y
# matrice des infos alléliques recodé en 0/1/2
Y-1
# on remarque que le marqueur 4 est associé à la pop 2
table(pop,Y[,4]) # cohérent avec les paramètres de simu... 

## modele M2 avec admixture
# paramètres
n=12
K=2
q=rdirichlet(n,c(1/2,1/2))
#pop=apply(q,1,which.max) # toujours ok
L=5
G1=cbind(c(1/3,1/3,1/3),
         c(4/6,1/6,1/6),
         c(3/16,7/16,6/16),
         c(4/10,1/10,5/10),
         c(49/60,10/60,1/60))
G2=cbind(c(3/16,7/16,6/16),
         c(1/3,1/3,1/3),
         c(4/10,1/10,5/10),
         c(49/60,10/60,1/60),
         c(4/6,1/6,1/6))

## simulation de S
S <- matrix(nrow=n,ncol=L)
for(i in 1:n){
  for(l in 1:L){
    S[i,l] <- which.max(rmultinom(n=1,size=1,q[i,]))
  }
}
S
pop



## simulation des données Y
Y <- matrix(ncol=L,nrow=n)
for(i in 1:n){
  for(l in 1:L){
    if(S[i,l]==1){
      Y[i,l] <- which.max(rmultinom(n=1,size=1,prob = G1[,l]))
    } else {
      Y[i,l] <- which.max(rmultinom(n=1,size=1,prob = G1[,l]))
    }
  }
}

Y-1

## on vérifie que l'on a bien,
## pour chaque individu i 
## et chaque locus l 
sum(q[i,1]*G1[,l]+q[i,2]*G2[,l])

## --------------------------------------------------------------------------###

## Apperçu sur les chaines de Markov
# ------------------------------------------------------------------------------
library(markovchain)
help("markovchain-class")
#show markovchain definition
statesNames <- c("pluie", "soleil")
#create a simple Markov chain
transMatr<-matrix(c(0.7,0.3,.6,.4),nrow=2,byrow=TRUE)
mcB<-new("markovchain", states=statesNames,
              transitionMatrix=transMatr, 
              name="simpleMc")
mcB
## Voir la séquence
outs <- markovchainSequence(n=100, markovchain = mcB, t0 = "pluie")
outs

# Mesure stationnaire
steadyStates(mcB)

## Comprendre les HMM
# ------------------------------------------------------------------------------
library(HMM)
statesNames = c("pluie", "soleil")
activites = c("marche", "lecture")
initialmeasure = c(.5, .5)
P = matrix(c(.7, .3, .6, .4), ncol = 2, byrow = T)
P
G = matrix(c(.2, .8, .9, .1), ncol = 2, byrow = T)
G
myHMM <- initHMM(States = statesNames, activites, initialmeasure, P, G)
print(myHMM)
simHMM(myHMM, 10)

# il ressemble à un modèle de mélange mais il faut noter ici que les observations sont indépendantes 
##### contrairement à ce qu'on retrouve dans un modèle de mélange classique
## mainenant l'algo forwoard bacwoard permettent de retrouver les params du modèle HMM 
## algo importants :"baumWelch", "Viterbi"
# Données observées
observations <- simHMM(myHMM, 100)$observations

# Initialisation aléatoire des paramètres
initialProbs <- rep(1/length(statesNames), length(statesNames))
transitionProbs <- matrix(runif(length(statesNames)^2), nrow=length(statesNames))
transitionProbs <- apply(transitionProbs, 1, function(x) x/sum(x))
emissionProbs <- matrix(runif(length(statesNames)*length(activites)), nrow=length(statesNames))
emissionProbs <- apply(emissionProbs, 1, function(x) x/sum(x))

# Estimation des paramètres avec l'algorithme de Baum-Welch
estimatedHMM <- baumWelch(observations, initialProbs, transitionProbs, emissionProbs)

# Affichage des paramètres estimés
print(estimatedHMM)

