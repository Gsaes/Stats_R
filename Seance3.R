#Exercice 1
tab <- read.csv("/Users/guillaumesaes/Desktop/Enseignement UMONS/Probabilités/all_seasons.csv", header = TRUE, sep= ",", encoding="UTF-8")
taille <- tab["player_height"][,1]
poids <- tab["player_weight"][,1]

hist(taille)
hist(poids)

borne1 <- -c(qnorm((1-0.95)/2,0,1),qnorm((1-0.99)/2,0,1))

Int_taille1 <- t.test(taille, mu=mean(taille))[4]
Int_poids1 <- t.test(poids, mu=mean(poids))[4]

Int95_taille2 <- c(mean(taille)-borne1[1]*(sd(taille)/sqrt(length(taille))),mean(taille)+borne1[1]*(sd(taille)/sqrt(length(poids))))
Int95_poids2 <- c(mean(poids)-borne1[1]*(sqrt(length(poids))/sd(poids)),mean(poids)+borne1[1]*(sd(poids)/sqrt(length(poids))))
Int99_taille2 <- c(mean(taille)-borne1[2]*(sd(taille)/sqrt(length(taille))),mean(taille)+borne1[2]*(sd(taille)/sqrt(length(taille))))
Int99_poids2 <- c(mean(poids)-borne1[2]*(sd(poids)/sqrt(length(poids))),mean(poids)+borne1[2]*(sd(poids)/sqrt(length(poids))))

#Exercice 2

data <- c(71.03, 74.38, 65.78, 64.72, 74.70, 66.22 , 72.68 , 66.90, 69.45 , 69.93, 68.62, 68.40)

borne2 <- c(qchisq((1-0.95)/2,length(data)-1),qchisq(0.975,length(data)-1))
Int_sigma <- 1/sqrt(c(borne2[2]/((length(data)-1)*sd(data)^2),borne2[1]/((length(data)-1)*sd(data)^2)))

#Exercice 3

note <- c(24.00, 28.00, 27.75, 25.00, 24.25, 23.50, 26.25, 24.00, 25.00, 30.00, 23.25, 26.25, 21.50, 26.00, 28.00, 24.50, 22.50, 28.25, 21.25, 19.75)

borne3 <- -c(qnorm((1-0.99)/2,0,1),qnorm((1-0.95)/2,0,1),qnorm((1-0.90)/2,0,1))

Int99s_note <- t.test(note,mu=mean(note), conf.level=0.99)[4]
Int95s_note <- t.test(note,mu=mean(note), conf.level=0.95)[4]
Int90s_note <- t.test(note,mu=mean(note), conf.level=0.9)[4]

Int99as_note <- t.test(note,mu=mean(note), conf.level=0.99, alternative ="greater")[4]
Int95as_note <- t.test(note,mu=mean(note), conf.level=0.95, alternative ="greater")[4]
Int90as_note <- t.test(note,mu=mean(note), conf.level=0.9, alternative ="greater")[4]

#Int99s_note <- c(mean(note)-borne3[1]*(sd(note)/sqrt(length(note))),mean(note)+borne3[1]*(sd(note)/sqrt(length(note))))
#Int95s_note <- c(mean(note)-borne3[2]*(sd(note)/sqrt(length(note))),mean(note)+borne3[2]*(sd(note)/sqrt(length(note))))
#Int90s_note <- c(mean(note)-borne3[3]*(sd(note)/sqrt(length(note))),mean(note)+borne3[3]*(sd(note)/sqrt(length(note))))

#Int99as_note <- mean(note)+qnorm(1-0.99,0,1)*(sd(note)/sqrt(length(note)))
#Int95as_note <- mean(note)+qnorm(1-0.95,0,1)*(sd(note)/sqrt(length(note)))
#Int90as_note <- mean(note)+qnorm(1-0.90,0,1)*(sd(note)/sqrt(length(note)))

