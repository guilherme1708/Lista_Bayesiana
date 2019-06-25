# Lista 5 - MAE0524

library(LearnBayes)

# Exercício 3

# item a

x <- c(10.2,19.0,22.3,23.8,24.9,26.1,26.9,27.9,28.5,29.2,30.1,31.0,31.7,
       32.4,32.9,33.3,34.1,34.6,35.5,36.3,37.0,38.0,39.0,39.6,43.0)

y <- c(0.01,0.09,0.12,0.18,0.19,0.23,0.24,0.31,0.33,0.32,0.33,0.40,0.45,
       0.49,0.67,0.48,0.54,0.63,0.59,0.65,0.50,0.81,0.87,0.71,0.91)

lx <- log(x)
ly <- log(y)

par(mfrow=c(1,2))
plot(y~x, xlab="Comprimento", ylab="Peso", main="Diagrama de dispersão", pch=19)
plot(ly~lx, xlab=expression(paste(log,"(Comprimento)")), ylab=expression(paste(log,"(Peso)")), main="Diagrama de dispersão", pch=19)

# item b

data <- data.frame(log(x),log(y))

# para conferir as estimativas com o modelo clássico
model <- lm(data$log.y. ~ data$log.x.)

logpost <- function(theta,data){ # log da posteriori de theta
  beta.0 <- theta[1]
  beta.1 <- theta[2]
  x <- data[,1]
  y <- data[,2]
  n <- length(x)
  -1/2*sum((y-beta.0-beta.1*x)^2)
}

laplace <- laplace(logpost,c(-12,3),data) # aprox pela normal

proposal <- list(var=laplace$var, scale=2)
start <- array(c(2.87,-0.03),c(1,2))
m <- 1000 # tamanho da amostra
s <- rwmetrop(logpost, proposal, start,m,data)
# item c

s.0 <- data.frame(exp(s$par[,1]),s$par[,2])

# para beta.1
quantile(s.0[,2],c(.05,.95))

# para beta.0
quantile(s.0[,1],c(.05,.95))

# item d

s.1 <- (s.0[,2]/(10^5*s.0[,1]))
hist(s.1, main=" Histograma da distribuição",probability = T,xlab=expression(beta[1]/10^5*beta[0]))

# IC
quantile(s.1,c(.05,.95))

# Exercício 4

# item a

data.x <- c(seq(1:18))
data.y <- c(15,11,14,17,5,11,10,4,8,10,7,9,11,3,6,1,1,4)
Data <- data.frame(data.x,data.y)

log.post <- function(theta,data){ # log da posteriori de theta
  beta.0 <- theta[1]
  beta.1 <- theta[2]
  i <- data[,1]
  y <- data[,2]
  sum(y*(beta.0 + beta.1*i)- exp(beta.0 + beta.1*i))
}

# item b

laplace <- laplace(log.post,c(9.5,5),Data) # aprox pela normal
proposal <- list(var=laplace$var, scale=2)
start <- array(c(2.87,-0.03),c(1,2))
m <- 1000 # tamanho da amostra
s <- rwmetrop(log.post, proposal, start,m,Data)

# verificar se bate com o modelo classico
mod.pois = glm(Data$data.y ~ Data$data.x, family=poisson(link='log'))
summary(mod.pois)

boxplot(s$par[,2], ylab=expression(beta[1]), main="Box-Plot dos valores simulados")

# item c

quantile(s$par[,2],c(.05,.95))

# Exercício 6

# item a

log.post <- function(theta,data){ # log da posteriori de theta
    y <- data
    n <- 5
    k <- length(data)
    theta*(sum(y)-8*theta)-n*k*log(1+exp(theta))
}

dados <- c(5)
aproxnorm <- laplace(log.post,0.5,dados)
aproxnorm

1-pnorm(0,aproxnorm$mode,sqrt(aproxnorm$var))

# item b
start <- array(c(aproxnorm$mode,2*sqrt(aproxnorm$var)),c(1,1))
proposal <- list(var=2*aproxnorm$var, scale=2)
m <- 10000 # tamanho da amostra
s <- rwmetrop(log.post, proposal, start,m,dados)

sum(s$par>0)/length(s$par)
