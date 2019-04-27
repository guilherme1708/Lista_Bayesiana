# Lista 3 - Análise Bayesiana de Dados - MAE0514

library(TeachingDemos)
library(LearnBayes)

# Exercício 3
amostra <- c(3,4,2,1,2,3)
A <- 17.25 # parâmetro de forma da distr Gamma
B <- 6.75 # parâmetro de escala da distr Gamma

hpd(qgamma,shape=17.25,rate=6.75,conf = 0.95) # Intervalo HPD p/ theta

c(qgamma(0.025,A,B),qgamma(0.975,A,B)) # Intervalo de caudas iguais p/ theta

c(qchisq(0.025,34.5)/13.5,qchisq(0.975,34.5)/13.5) # Intervalo de caudas iguais p/ theta utilizando a distr X²

# Exercício 5
dados <- c(8, 23, 22, 2, 4, 3, 11, 4, 23, 21)

# item b
ln.dados <- log(dados) # log dos dados

log_posterior <- function(phi, data){ # log da posteriori de phi
  lambda <- exp(phi)
  n <- length(dados)
  y <- data
  logf <- function(y,n, lambda){
    log(lambda^n*exp(-lambda*sum(y)))
  }
  val <- logf(y,n, lambda) + phi
  return(val)
}

s<-laplace(log_posterior,1,ln.dados) # testando se os resultados teóricos bate com o computaiconal

# item d

# Gráficos
a <- 1/10
b <- 1/10
n <- length(dados)
x.bar <- mean(dados)
A <- a+n
B <- b+n*x.bar

curve(dgamma(x,A,B),ylim=c(0,17), from=0, to=0.2, ylab="Densidade a Posteriori",xlab=expression(lambda))
curve(dnorm(x,(A-1)/(B),sqrt(A-1)/(B)),lwd=2,col=1,lty=3,add = T)
curve(dlnorm(x,log(A/B),1/sqrt(A)),lwd=2,col=1,lty=2,add = T)
l<-c("Gama","Normal","Log-Normal")
legend("topright",l,lwd=2,lty=c(1,2,3),bty="n",cex=1.2)

# Exercício 6

# item a 
log_posterior <- function(theta,dados){ # log da posteriori de theta
  n <- length(dados) # tamanho dos dados
  mu <- theta[1] # parametro mu
  sigma2 <- theta[2] # parametro sigma²
  x <- dados # dados
  -n/2*log(sigma2)-1/(2*sigma2)*sum((x-mu)^2)-log(sigma2)-(mu-300)^2/200
}

data("marathontimes")
attach(marathontimes)
mycontour(log_posterior,c(240,330, 100, 10000), time, xlab = "Média", ylab = "Variância",
          main = "Gráfico de contornos")

# item b

n <- length(time) # Tamanho da amostra
S <- (n-1)*var(time)# soma dos desvio da média ao quadrado
sigma2 <- S/rchisq(1000, n-1) # simulando variâncias
mu <- rnorm(1000,mean(time),sqrt(sigma2/n)) # simulando médias
mycontour(log_posterior,c(240,330, 100, 10000), time, xlab = "Média", ylab = "Variância",
          main = "Gráfico de contornos com simulação")
points(mu, sigma2) # Colocando os pontos no gráfico

# item c
cv <- mu/sqrt(sigma2) # coeficiente de variação
quantile(cv, c(0.05, 0.25, 0.5, 0.75, 0.95)) # quantis pedidos
hist(cv, freq = F, ylab = "Densidade", xlab = "Valores do CV", main = "Histograma do CV")

