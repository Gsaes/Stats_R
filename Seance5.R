#Exercice 1

# Test d'hypothèse bilatéral
# H0 : delta=mu-nu <= 0
# H1 : delta=mu-nu > de 0
# Marge d'erreur 5%

x1 <- c(0.0011, 0.0014, 0.0018, 0.0022, 0.0010, 0.0016, 0.0028, 0.0020, 0.0015, 0.0014, 0.0023, 0.0017, 0.0020)
y1 <- c(0.0011, 0.0010, 0.0019, 0.0013, 0.0011, 0.0017, 0.0024, 0.0020, 0.0013, 0.0013, 0.0017, 0.0015, 0.0013)

# Test de Fischer-Snedecor
# H0 : sigma_X = sigma_Y
# H1 : sigma_X différent sigma_Y
# Marge d'erreur 5%
print('La p-valeur du Test de Fischer est :')
print(var.test(x1,y1)$p.value)
print('On accepte l\'hypothèse sigma_X=sigma_Y')

print('La p-valeur vaut :')
print(t.test(x1,y1,var.equal = TRUE, alternative = 'greater')$p.value)
print('On ne rejete pas l\'hypothèse H0')

#Exercice 2

# Test d'hypothèse bilatéral
# H0 : delta=mu-nu = 0
# H1 : delta=mu-nu différent de 0
# Marge d'erreur 5%

x2<- c(12.12, 12.03, 13.58, 13.38, 11.81, 15.92, 13.65)
y2<- c(14.81, 13.93, 14.91, 15.87, 15.62, 15.39)

# Test de Fischer-Snedecor
# H0 : sigma_X = sigma_Y
# H1 : sigma_X différent sigma_Y
# Marge d'erreur 5%
print('La p-valeur du Test de Fischer est :')
print(var.test(x2,y2)$p.value)
print('On rejète significativement l\'hypothèse sigma_X=sigma_Y')

print('La p-valeur vaut :')
print(t.test(x2,y2,var.equal = FALSE)$p.value)
print('On rejete significativement l\'hypothèse H0')

#Exercice 3

# Test d'hypothèse bilatéral (appariés)
# H0 : delta=mu-nu = 0
# H1 : delta=mu-nu différent de 0
# Marge d'erreur 5%
x3 <- c(68.3, 60.1, 52.2, 41.7, 32.0, 30.9, 39.3, 42.0, 37.7, 33.5, 32.2, 63.3, 54.2, 47.0, 91.9, 56.1, 79.6, 81.2, 78.4, 46.6)
y3 <- c(72.5, 56.0, 55.8, 39.2, 31.4, 35.5, 39.2, 41.1, 43.3, 31.7, 31.9, 58.1, 52.7, 46.2, 90.2, 55.4, 75.1, 86.6, 75.3, 43.8)

# Test de Fischer-Snedecor
# H0 : sigma_X = sigma_Y
# H1 : sigma_X différent sigma_Y
# Marge d'erreur 5%
print('La p-valeur vaut :')
print(var.test(x3,y3)$p.value)
print('On accepte H0 sans conviction')

print('La p-valeur vaut :')
print(t.test(x3,y3, paired = TRUE, var.equal = TRUE)$p.value)
print('Il semble que la densité d\'écorce soit la même des deux côtés des arbres')


#Exercice 4
# Test du khi-2
# Marge d\'erreur de 1%
x4<-c(180, 190, 160, 140, 220, 110)
print('La p-valeur vaut :')
print(chisq.test(x4)$p.value)
print('Le dé est clairement truqué avec une marge d\'erreur de 1%')

dé<-function(n){
  return(floor(runif(n,1,7)))
}

y4<- dé(1000)
print('La p-valeur vaut :')
print(chisq.test(table(y4))$p.value)
print('Le dé ne semble pas truqué sans conviction')
set.seed(0)
z4<- sample(c(1,2,3,4,5,6),1000,replace=T,prob=c(.15,.15,.15,.17,.19,.19))
print('La p-valeur vaut :')
print(chisq.test(table(z4))$p.value)
print('Le dé est truqué avec une marge d\'erreur de 1%')

#Exercice 5

aj <- 0:6
nj <- c(37, 46, 39, 19, 5, 3, 1)
n <- sum(nj)
lamb <- sum(aj*nj)/n
cj <- n*dpois(aj,lambda = lamb)

bj <- 0:4
mj <- c(37, 46, 39, 19, 9)

print(chisq.test(mj,p=dpois(bj,lambda = lamb),rescale.p=TRUE))
print('Donc nos données suivent une loi de Poisson de paramètre :')
print('lambda')