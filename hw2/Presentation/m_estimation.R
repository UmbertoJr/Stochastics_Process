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


function.M <- function(obs, M){
  n = length(obs)
  Z = length(unique(obs))
  num = Z*log(M)
  den = log(M)
  for(i in 1:(n-1)){
    den = den + log(M + i)
  }
  return(num - den)
}

par(mfrow=c(1,1))
M = 10
number.obs=100
observed <- sethuraman.cost(number.obs,M=M)
uniq.obs <- unique(observed)
plot(density(uniq.obs))


xx <- seq(from = 0 , to = 150, by = 0.1)
value <- function.M(observed, xx)
plot(xx, value,type = 'l', col= rgb(1,114,116,250,maxColorValue = 255), main = 'Likelihood estimation of M')
m.bar <- xx[which.max(value)]
segments(m.bar,-1e10,m.bar,1e10,col= rgb(1,114,116,250,maxColorValue = 255))
segments(M,-1e10,M,1e10,col = 'red')
legend('topright',
       legend = c(paste('Estimated M = ',round(m.bar,1)),
                  paste('True M = ',M)),
       cex = .7)



M <- c(5, 20 , 50, 100)
number.obs=100
sim= 100
m.estimated <- list()
for(i in 1:4){
  m.estimated[[i]] <- rep(0, sim)
  for(z in 1:sim){
    observed <- sethuraman.cost(number.obs,M=M[i])
    value <- function.M(observed, xx)
    m.estimated[[i]][z] <- xx[which.max(value)]
  }
}

#save(m.estimated, file = 'Stima_m.RData')
load(file = 'Stima_m.RData')

par(mfrow = c(2,2), mar =c(2,2,2,1))
for(i in 1:4){
  hist(m.estimated[[i]], main = 'distribution of M', probability = T, col = 'blue')
  lines(density(m.estimated[[i]]))
  m <- mean(m.estimated[[i]])
  legend('topright',
         legend = substitute(paste(bar(M),' = ',m), list(m = round(m,1))),
         cex = 0.5)
}


compute.prob <- function(obs, M){
  lo <- function.M(obs, M)
  un.obs <- unique(obs)
  num = 0
  den = 0
  m.vec = rep(0, length(obs))
  for(el in un.obs){
    m.vec[sum(el == obs)] = m.vec[sum(el == obs)] + 1
  }
  for(i in (1:length(obs))){
    num = num + log(i)
    den = den + m.vec[i]*log(i) + log(factorial(m.vec[i]))
  }
  return(lo + num - den)
}

exp(compute.prob(observed, 11.1)) 
exp(compute.prob(observed, 10))

