# Lista de Bayesiana

library(invgamma)
library(LaplacesDemon)
library(LearnBayes)

# Exercicio 4
x.points <- seq(-3,3,length.out=100) # Gride de pontos no eixo x
y.points <- x.points # Gride de pontos no eixo x
z <- matrix(0,nrow=100,ncol=100)

contourTmulti <- function(b,lambda,k,bla){
  for (i in 1:100) {
    for (j in 1:100) {
      z[i,j] <- dmvt(c(x.points[i],y.points[j]), lambda, matrix(c(1,b,b,1),2,2), df=k)
    }
  }
  contour(x.points,y.points,z,lwd = 2,xlab = expression(theta[1]), ylab = expression(theta[2]), main = bla)
}

par(mfrow=c(2,2))
k <- 3
lambda <- c(0,0)

contourTmulti(-0.9,lambda,k,bla=c(expression(paste(paste("T-student Bivariada "),"Cov(",paste(theta[1]),paste(",",theta[2]),paste(")"),paste("=-0.9")))))
contourTmulti(0,lambda,k,bla=c(expression(paste(paste("T-student Bivariada "),"Cov(",paste(theta[1]),paste(",",theta[2]),paste(")"),paste("=0")))))
contourTmulti(0.2,lambda,k,bla=c(expression(paste(paste("T-student Bivariada "),"Cov(",paste(theta[1]),paste(",",theta[2]),paste(")"),paste("=0.2")))))
contourTmulti(0.5,lambda,k,bla=c(expression(paste(paste("T-student Bivariada "),"Cov(",paste(theta[1]),paste(",",theta[2]),paste(")"),paste("=0.5")))))

# Exercicio 6

# Dados
D <- c(22.0, 23.9, 20.8, 23.8, 25.0, 24.0, 21.7, 23.8, 22.8, 23.1)

RW <- c(23.2, 22.0, 22.2, 21.2, 21.6, 21.9, 22.0, 22.9, 22.8)

# Tamanho dos dados
nD <- length(D)
nRW <- length(RW)

# Desvio-padrão dos dados
sD <- sqrt(nD/(nD-1)*var(D))
sRW <- sqrt(nRW/(nRW-1)*var(RW))

# Média dos dados
mD <- mean(D)
mRW <- mean(RW)

mcDiff <- numeric()

for (i in 1:10000){
  mcD <- rinvgamma(1, (nD-1)/2, (nD-1)*sD^2/2)
  mcRW <- rinvgamma(1, (nRW-1)/2, (nRW-1)*sRW^2/2)
  aux <- rnorm(1, mD-mRW, mcD/nD + mcRW/nRW)
  mcDiff <- c(mcDiff, aux)
}

quantile(mcDiff, c(0.05, 0.95))

# Exercício 7

y <- c(40,36,40,38,42,39,40,37,36,38,39,40)
x <- c(3317,2729,2935,2754,3210,2817,3126,2539,2412,2991,2875,3231)
dados <- data.frame(x,y)

log_posterior <- function(theta,data){ # log da posteriori de theta
  a <- theta[1]
  b <- theta[2]
  x <- data[,1]
  y <- data[,2]
  n <- length(y)
  x_bar <- mean(x)
  y_bar <- mean(y)
  Sxx <- sum((x-x_bar)^2) 
  Syy <- sum((y-y_bar)^2)
  Sxy <- sum((x-x_bar)*(y-y_bar))
  Srr <- Syy*(1-Sxy^2/(Sxx*Syy)) 
  alf <- mean(y)
  bet <- Sxy/Sxx
  sigma2 <- Srr/(n-4)
  -n*(alf-a)^2/(2*sigma2)-Sxx*(bet-b)^2/(2*sigma2)
}
mycontour(log_posterior,c(37,41,0,0.012),dados, xlab = "Beta0", ylab = "Beta1",
          main = "Gráfico de contornos")

s <- simcontour(log_posterior,c(37,41,0,0.012),dados,1000)
points(s)
