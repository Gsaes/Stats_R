# Exercice 1
n <- 24
data <- c(6.47,7.02,7.15,7.22,7.44,6.99,7.47,7.61,7.32,7.22,7.52,6.92,7.28,6.69,7.24,7.19,6.97,7.52,6.22,7.13,7.32,7.67,7.24,6.21)
mu0 <- 7.3

# Test d'hypothèse bilatéral
# H0 : mu = mu0
# H1 : mu différent de mu0
# Marge d'erreur 5%

# Sous l'hypothèse H0
pval95 <- t.test(data, mu=mu0)$p.value

# Conclusion
ifelse(pval95<0.05, 
       {print('L\'hypothèse H0 est rejeté significativement : on peut affirmer que le fournisseur ne respecte pas ses engagements.')},
       {print('L\'hypothèse H0 ne peut pas être rejeté significativement : on ne peut pas affirmer que le fournisseur ne respecte pas ses engagements.')})
print('Il y a environ une probabilité de 4,1% de se tromper.')

# Exercice 2
n <- 5
data <- c(5.1,3.0,3.6,4.8,3.4)
mu0 <- 5
# Test d'hypothèse unilatéral
# H0 : mu >= mu0
# H1 : mu < mu0
# Marge d'erreur 4%

# Sous l'hypothèse H0
pval96 <- t.test(data, mu=mu0, alternative = "less", conf.level = 0.96)$p.value

# Conclusion
ifelse(pval96< 0.04, 
       {print('L\'hypothèse H0 est rejeté significativement')},
       {print('L\'hypothèse H0 ne peut pas être rejeté significativement.')})
print('Il y a environ une probabilité de 3,4% de se tromper.')

# Exercice 3
n <- 12
data <- c(10.1,9.8, 10.2, 10.3, 10.4, 9.8, 9.9, 10.4, 10.2, 9.5, 10.4, 9.6)
mu0 <- 10

# Test d'hypothèse unilatéral
# H0 : mu <= mu0
# H1 : mu > mu0
# Marge d'erreur 5%

# Sous l'hypothèse H0
pval95_2 <- t.test(data, mu=mu0, alternative = "greater")$p.value

# Conclusion
ifelse(pval95_2< 0.05, 
       {print('L\'hypothèse H0 est rejeté significativement')},
       {print('L\'hypothèse H0 ne peut pas être rejeté significativement.')})
print('Les données ne nous permettent pas d’affirmer que le contenu moyen des récipients de cette usine est strictement supérieur à 10 litres')

# Exercice 4
n <- 12
data <- c(24.00, 28.00, 27.75, 25.00, 24.25, 23.50, 26.25, 24.00, 25.00, 30.00, 23.25, 26.25, 21.50, 26.00, 28.00, 24.50, 22.50, 28.25, 21.25, 19.75)
sd0 <- 5
sd1 <- sd(data)

# Test d'hypothèse unilatéral
# H0 : sd >= sd0
# H1 : mu <sd0
# Marge d'erreur 10%, 5%, 1%

pval4 <-pchisq(((n-1)/(sd0^2))*sd(data)^2,n-1)
print('La p-value est de : ')
print(pval4)

reg1 <- c(-Inf,sqrt(qchisq(0.01,n-1)*(sd0^2/(n-1))))
reg5 <- c(-Inf,sqrt(qchisq(0.05,n-1)*(sd0^2/(n-1))))
reg10 <- c(-Inf,sqrt(qchisq(0.10,n-1)*(sd0^2/(n-1))))
print('La région de rejet avec une marge de 1% est : ')
print(reg1)
print('La région de rejet avec une marge de 5% est : ')
print(reg5)
print('La région de rejet avec une marge de 10% est : ')
print(reg10)

print('Donc l\'hypothèse H0 est rejeté significativement avec une marge de 5% et 10%')
print('Donc l\'hypothèse H0 n\'est pas rejeté significativement avec une marge de 1%')

# Exercice 5
n <- 5000
mu0 <- 0.01
sd0 <- sqrt(0.01*(1-0.01))
mu1 <- 0.014
sd1 <- sqrt(0.014*(1-0.014))

# Test d'hypothèse unilatéral
# H0 : mu <= mu0
# H1 : mu > mu0
# Marge d'erreur 10%, 5%, 1%

pval <-1-pnorm((0.014-0.01)/(sqrt(0.014*(1-0.014))/sqrt(5000)),0,1)
print('La p-value est de ')
print(pval)
print('Donc l\'hypothèse H0 est rejeté significativement')

r08 <- c(mu0+qnorm(1-0.008,0,1)*(sd1/sqrt(n)),Inf)
r1 <- c(mu0+qnorm(1-0.01,0,1)*(sd1/sqrt(n)),Inf)
r5 <- c(mu0+qnorm(1-0.05,0,1)*(sd1/sqrt(n)),Inf)
r10 <- c(mu0+qnorm(1-0.10,0,1)*(sd1/sqrt(n)),Inf)
print('La région de rejet avec une marge de 0,8% est : ')
print(r08)
print('La région de rejet avec une marge de 1% est : ')
print(r1)
print('La région de rejet avec une marge de 5% est : ')
print(r5)
print('La région de rejet avec une marge de 10% est : ')
print(r10)
