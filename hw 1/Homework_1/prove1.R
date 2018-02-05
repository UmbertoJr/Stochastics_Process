ferguson.def <- function(number.obs, M){
  sample <- seq(from=-5, to=5, length.out = number.obs)
  par <- rep(0,number.obs)
  sim <- rep(0,number.obs)
  par[1] <- M*pnorm(sample[1])
  sim[1] <- rgamma(1,par[1])
  for(i in 2:number.obs){
    par[i] <- M*(pnorm(sample[i]) - pnorm(sample[i-1]))
    sim[i] <- rgamma(1,par[i])
  }
  tot <- sum(sim)
  ret <- sim/tot
  return(ret)
}

number.obs=100
a <- ferguson.def(number.obs,M=10)

sample <- seq(from=-5, to=5, length.out = number.obs)
matplot(sample, cumsum(a))
curve(pnorm, add=T, col='blue')


M <- c(5, 20 , 50, 100)
number.obs=1000
sim= 1000
a <- list()
for(i in 1:4){
  a[[i]] <- matrix(nrow = sim, ncol = number.obs)
  for(l in 1:sim)
    a[[i]][l,] <- ferguson.def(number.obs,M=M[i])
}
save(a, file = 'ferguson.Rdata')


sample <- seq(from=-5, to=5, length.out = number.obs)
par(mfrow=c(2,2), mar=c(2,2,2,1))
for(i in 1:4){
  el <- apply(a[[i]],1,cumsum )
  matplot(sample,el, type = 'l', lwd = 0.00001, lty=3,main = paste('M=', M[i]))
  curve(pnorm, add=T, col='black', lwd=2)
}

mu.fun <- list(rep(0,sim),rep(0,sim),rep(0,sim),rep(0,sim))
var.fun <- list(rep(0,sim),rep(0,sim),rep(0,sim),rep(0,sim))
for(m in 1:4){
  for(i in 1:sim){
    mu.fun[[m]][i] <- sample%*%a[[m]][i,]
    var.fun[[m]][i] <- (sample)^2%*%a[[m]][i,] - (sample%*%a[[m]][i,])^2
  }
  plot(density(mu.fun[[m]]), main = paste('mean.fun M=', M[m]))
}

for(m in 1:4){
  plot(density(var.fun[[m]]), main = paste('var.fun M=', M[m]))
}






# sethuraman --------------------------------------------------------------


sethuraman.cost <- function(number.obs, M){
  n <- 5000
  y <- rnorm(n)
  thet <- rbeta(n,shape1 = 1, shape2 = M)
  prob <- rep(0,n)
  prob[1] <- thet[1]
  for(i in 2:n){
    prob[i]<- thet[i]*prod(1 - thet[1:i-1])
  }
  dat <- sample(y,size= number.obs, prob=prob,replace=T)
  return(dat)
}

dat <- sethuraman.cost(100,10)
plot(ecdf(dat))
curve(pnorm,add=T)


M <- c(5, 20 , 50, 100)
number.obs=100
sim= 100
a <- list()
for(i in 1:4){
  a[[i]] <- matrix(nrow = sim, ncol = number.obs)
  for(l in 1:sim)
    a[[i]][l,] <- sethuraman.cost(number.obs,M=M[i])
}

#save(a, file = 'sethuraman2.RData')

#load(file = 'sethuraman.RData')

require(randomcoloR)


par(mfrow=c(2,2), mar=c(2,2,2,1))
for(i in 1:4){
  curve(pnorm, col='black', lwd=2, from = -4, to=4)
  for(l in 1:100){
    plot(ecdf(a[[i]][l,]),verticals=TRUE, do.points=FALSE, lwd = 0.00001, lty=1,main = paste('M=', M[i]), add=T,col=randomColor())
  }
  curve(pnorm, add=T, col='black', lwd=2)
}

mu.fun <- list(rep(0,sim),rep(0,sim),rep(0,sim),rep(0,sim))
var.fun <- list(rep(0,sim),rep(0,sim),rep(0,sim),rep(0,sim))
for(m in 1:4){
  for(i in 1:sim){
    mu.fun[[m]][i] <- mean(a[[m]][i,])
    var.fun[[m]][i] <- var(a[[m]][i,])
  }
  plot(density(mu.fun[[m]]), main = paste('mean.fun M=', M[m]))
}

for(m in 1:4){
  plot(density(var.fun[[m]]), main = paste('var.fun M=', M[m]))
}




# MDP ---------------------------------------------------------------------
sim = 100
G <- matrix(nrow = sim,ncol = number.obs)
for(t in 1:sim){
  M <- rgamma(1,3,3)
  G[t,] <- sethuraman.cost(number.obs,M)
}
save(G, file = 'MDP.RData')

par(mfrow=c(1,2), mar=c(3,3,3,3))
curve(pnorm, col='black', lwd=2, from = -4, to=4)
for(l in 1:100){
  plot(ecdf(G[l,]),verticals=TRUE, do.points=FALSE, lwd = 0.00001, lty=1,main = 'M sim from gamma', add=T,col=randomColor())
}
curve(pnorm, add=T, col='black', lwd=2)

curve(dnorm, col='black', lwd=2, from = -4, to=4)
for(l in 1:100){
  lines(density(G[l,]), lwd = 0.00001, lty=1,main ='M sim from gamma',col=randomColor())
}
curve(dnorm, add=T, col='black', lwd=2)


mu.fun <- rep(0,sim)
var.fun <- rep(0,sim)

for(i in 1:sim){
  mu.fun[i] <- mean(G[i,])
  var.fun[i] <- var(G[i,])
}
plot(density(mu.fun), main = 'mean distribution')
plot(density(var.fun), main = 'variance distribution')




