## Préliminaire

# Structure d'une fonction
f<-function(x) { #nom<-function(variable) {instruction}
  y<-x^2
}

# Structure d'un plot
plot(t,f(t),xlab = 'Axe des abscisses', ylab='Axe des ordonnées', main='Représentation graphique de la fonction carée')

## Exercice 1

# Réalisation d'une loi rnom(paramètre)
# Réalisation d'un histogramme hist(data,nombre de classe)

#La loi uniforme sur [1,20]
X <- runif(100000, min = 1, max = 20)
hist(X,100)
# La loi normale
Y <- rnorm(100000, mean = 0, sd = 1)
hist(Y,100)
# La loi binomiale
Z <- rbinom(100000, size = 50, prob = 0.1)
hist(Z,100)

## Exercice 2

# Question 1
# La fonction floor permet de garder la partie entière d'un nombre réel x
unifd<-function(n){
  return(floor(runif(n, min = 1, max = 7)))
}
X = unifd(10000)

# Question 2
hist(X,100)

# Question 3
# Nous allons utiliser la condition if(cond) expr
# Les opérateurs logiques ET et OU se note && et ||
dunifd<-function(x){
  if(x %in% c(1,2,3,4,5,6)) return(1/6) else return(0)
}

# Exercice Supp: Comparer la fréquence des (xi) et la loi de X pour différente de n

punifd <-function(x){
  ifelse(x<1,return(0),ifelse(x<2,return(1/6),ifelse(x<3,return(1/3),ifelse(x<4,return(1/2),ifelse(x<5,return(2/3),ifelse(x<6,return(5/6),return(1)))))))
}

# Finir l'exercice

## Exercice 3

# Si on souhaite obtenir une réalisation d'une variable X tel que la fonction de répartition F est bijective 
# alors on va effectuer des réalisations de la variable aléatoire F^(-1)(U) où U est une v.a. de loi uniforme sur [0,1].

# La fonction de réparition de la v.a. Z est F(x) = 1-exp(-\lambda x) si x>=0 et F(x) = 0 sinon
# La fonction de répartition admet une fonction inverse F^(-1) (y) = - ln(1-y)/\lambda

expc<-function(n,lambda){
  return(-log(1-runif(n))/lambda)
}

Z = expc(10000,0.2)
hist(Z,100)

Z2 = rexp(10000,0.2)
hist(Z2,100)

## Exercice 5

pareto<-function(n,alpha){
  return((1/(1-runif(n)))^(1/alpha)-1)
}

X = pareto(10000,10)
hist(X,100)

t<-1:10000
for(i in t) {x[i]<-mean((1-runif(i,0,1))^(-1/3)-1)}
plot(t,x,xlab= "Taille de l'échantillon",ylab= "Moyenne de l'échantillon",main="Représentation de la convergence de la moyenne empirique")
lines(c(0,1000),c(1/2,1/2), col="red")



