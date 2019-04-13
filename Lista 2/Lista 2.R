# Lista 2 - Análise Bayesiana de Dados - MAE0514

#4
library(LearnBayes)

x <- c(16.6,16.4,17.3,14.5,15.3,15.2,18.1,17.6,17.6,16.3,15.4,17.2)
#a       
sd<-normpostsim(x,prior=NULL,m=1000)


d <- mycontour(normchi2post, c(13,20, 0.1, 7), x,xlab="mean",ylab="variance")
S = sum((x - mean(x))^2)
n = length(x)
sigma2 = S/rchisq(1000, n - 1)
#b
mu = rnorm(1000, mean = mean(x), sd = sqrt(sigma2)/sqrt(n))
points(mu, sigma2)
#c
quantile(mu, c(0.05, 0.95))
quantile(sqrt(sigma2), c(0.05, 0.95))


#8       
y <- c(0,10,9,8,11,3,3,8,8,11)
#a
x <- seq(from=-2,to=12,by=0.1)

#b
prod <- rep(0,141)
for(j in 1:141){
  for (i in 1:10){
    prod[j] <- prod[j] + 1/(1+(y[i]-x[j])^2)
  }
}
summary(y)
plot(dcauchy(x,8.2,1))


#c


m=array(c(0,0),c(2,1))
mv=array(c(1,.6,.6,1),c(2,2))
