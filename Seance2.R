## Exercice 1

n = 1000
p0 = 1/3

x <- rbinom(n,1,p = p0) # On réalise n observations de notre variable aléatoire X
noui <- sum(x) # On note noui la somme des xi qui correspond aux nombres de "oui" ou personne ayant la caractéristique donnée
nnon <- n - noui
pest <- sum(x)/n # C'est notre estimateur

vraisemblance <- function(p) {
  (p^noui)*((1-p)^nnon) # C'est la fonction de vraisemblance dont on souhaite déterminer le max
}

delta <- 2.5*sqrt(pest*(1-pest)/n) # Ecart déterminer à partir de la règle 1,2,3 Sigma
xmin <- pest - delta
xmax <- pest + delta

ptheta <- seq(from = xmin, to = xmax, length = 50) # Variable \theta qu'on fait varier
y <- vraisemblance(ptheta) # Représente la fonction de Vraisemblance


listestim <-function(m){ # Simulation de m estimateurs
  pest <- replicate(m,mean(rbinom(n,1,p = p0)))
  return(pest) # Retour la liste des m estimateurs
}

# Représentation des différents graphiques
plot.new() 
par(mar=c(4,4,3,5)) 
hist(listestim(10000),20,axes=F, main ='', xlab='', ylab='')
axis( 1 ,col="black",col.axis="black",at=seq(0.25, 0.4, by=0.01))
axis( 2 ,col="black",col.axis="black",at=seq(0, 1500, by=300)) 

par(new = T)
y <- vraisemblance(ptheta)
plot(ptheta, y, type = "l", axes=F, main ='Histogramme des estimateurs de maximum de vraisemblance', xlab='Paramètre theta', ylab='')
axis( 4 ,col="black",col.axis="black",at=seq(0, max(y), by=max(y)/4)) 
abline(v = p0, lwd=3, col = "red")
abline(v = pest, col = "blue")
legend(xmin, max(y), c("p vrai","p obs."), lwd = c(3,1), col = c("red","blue"))
axis( 4 ,col="black",col.axis="black",at=seq(0, 0, by=0.005)) 

# Exercice 2 

n <- 1000
theta <- 0.5

listestim1 <-function(m){ # Simulation de m estimateurs avec la méthode des moments
  pest1 <- replicate(m,1/(1-mean(runif(n,0,1)^(1/theta)))-1)
  return(pest1) # Retour la liste des m estimateurs
}

listestim2 <-function(m){ # Simulation de m EMV
  pest2 <- replicate(m,-n/sum(log(runif(n,0,1)^(1/theta))))
  return(pest2) # Retour la liste des m estimateurs
}

# Représentation des différents graphiques

p1 = listestim1(10000)
p2 = listestim2(10000)
plot.new() 
par(mar=c(4,4,3,5)) 
hist(p1,50, col="blue", main= "Histogramme des distributions des estimateurs", xlab = "Paramètre theta", ylab = "Fréquence")
axis( 1 ,col="black",col.axis="black",at=seq(0.44, 0.56, by=0.01))
axis( 2 ,col="black",col.axis="black",at=seq(0, 500, by=50)) 
read_csv(file = "data/deputes.csv")par(new = T)
hist(p2,50, col="green", main="", xlab="", ylab="")
