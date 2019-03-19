#Lista de Bayesiana 

library(LearnBayes)

# Exercício 1

# Item a
midpt <- seq(0.05, 0.95, by = 0.1) # vetor de pontos médios dos intervalos
prior <- rep(1,10) # Valores a priori(Dist. Uniforme(0,1))
prior <- prior/sum(prior) # Normalização
p <- seq(0, 1, length = 500) # grade de valores de p no intervalo (0,1) 
curve(histprior(x,midpt,prior), from=0, to=1, ylab="Densidade a Priori",ylim=c(0,.3))
obs <- c(1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 1) # Valores observados de caras
s <- sum(obs) # Soma dos valores observados de caras
f <-20-s # oma dos valores observados de coroas

curve(histprior(x,midpt,prior) * dbeta(x,s+1,f+1), from=0, to=1, ylab="Densidade a Posteriori") 

# Item b
like <- dbeta(p, s+1, f+1) # Verossimilhança
post <- histprior(p, midpt, prior) * like # Posteriori
post <- post/sum(post) # Probabilidades a posteriori
ps <- sample(p, replace = TRUE, prob = post)
hist(ps, xlab="p", main="" ,breaks=30) # Histograma simulado

# Execício 12

# Item a
p <- seq(0, 1, length = 500)
a <- 1 # Primeiro parâmento da beta
b <- 1 # Segundo parâmento da beta
s <- 22 # Sucessos
f <- 7 # Fracassos 
prior <- dbeta(p,a,b) # Priori
like <- dbeta(p,s+1,f+1) # Verossimilhança
post <- dbeta(p,a+s,b+f) # Posteriori

# Gráficos
curve(dbeta(x,a,b),ylim=c(0,5), from=0, to=1, ylab="Densidade a Posteriori",xlab=expression(theta))
curve(dbeta(x,a+s,b+f),ylim=c(0,5),lwd=2,col=1,ylim=c(0,0.09),lty=2,add = T)
a<-c("Priori ","Posteriori")
legend("topleft",a,lwd=2,lty=c(1,2),bty="n",cex=1.3)

# Item b
qbeta(c(0.05,0.95),23,8) # Intevalo de credibilidade com 0.9

# Item c


# Item d
1-pbeta(0.6,23,8) # Probabilidade do theta superar 0.6

# Item e
ps <- rbeta(1000,23,8)
hist(ps, probability = T, xlab = "p", main = "Histograma simulado", ylab="Densidade de Frequência", breaks = 30)

# Item f
y <- rbinom(1000, 10, ps)
table(y)
freq <- table(y)
predprob <- freq/sum(freq)
Prob <- predprob[predprob=10]+ predprob[predprob=11]

