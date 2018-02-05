
# simulation of data ------------------------------------------------------
N <- 10000
components <- sample(1:3,prob=c(0.5,0.3,0.2),size=N,replace=TRUE)
mus <- c(2.5,0.5,1.5)
sds <- sqrt(c(0.5,0.7,2))

samples <- rnorm(n=N,mean=mus[components],sd=sds[components])

hist(samples, probability = T)

x <- seq(-20,20,0.01)
truth <- 0.5*dnorm(x,mean=mus[1],sd=sds[1])+
  0.3*dnorm(x,mean=mus[2],sd=sds[2])+0.2*dnorm(x,mean=mus[3],sd=sds[3])
lines(x,truth,col="red",lwd=2)
lines(density(samples), type = 'l', col='blue')


# posterior analysis -------------------------------------------------------
#save(samples.20,samples.200,samples.2000,file = 'mixtureofnormalsim.RData')
load(file = 'mixtureofnormalsim.RData')


ferguson.posterior <- function(samp, M){
  number.obs <- 1000
  sample <- seq(from=-5, to=5, length.out = number.obs)
  par <- rep(0,number.obs)
  sim <- rep(0,number.obs)
  par[1] <- M*pnorm(sample[1])+ sum(samp<=sample[1])
  sim[1] <- rgamma(1,par[1])
  for(i in 2:number.obs){
    par[i] <- M*(pnorm(sample[i]) - pnorm(sample[i-1])) + sum(samp<=sample[i] & samp > sample[i-1])
    sim[i] <- rgamma(1,par[i])
  }
  tot <- sum(sim)
  ret <- sim/tot
  return(ret)
}

risul <- ferguson.posterior(samples.2000,3)

sample <- seq(from=-5, to=5, length.out = 1000)


plot(sample,risul,add=T)
lines(x,truth,col="red",lwd=2)

plot(sample,cumsum(risul), type = 'l')
truth.cdf <- 0.5*pnorm(x,mean=mus[1],sd=sds[1])+
  0.3*pnorm(x,mean=mus[2],sd=sds[2])+0.2*pnorm(x,mean=mus[3],sd=sds[3])
lines(x,truth.cdf,col="red",lwd=2)



# sethuraman --------------------------------------------------------------

sethuraman.post <- function(samp, M){
  n <- 5000- length(samp)
  y <- rnorm(n)
  thet <- rbeta(5000,shape1 = 1, shape2 = M+ length(samp))
  prob <- rep(0,5000)
  prob[1] <- thet[1]
  for(i in 2:5000){
    prob[i]<- thet[i]*prod(1 - thet[1:i-1])
  }
  obs <- c(samp,y)
  dat <- sample(obs,size= number.obs, prob=prob,replace=T)
  return(dat)
}

risul <- sethuraman.post(samples.2000,3)

hist(risul, probability = T); lines(x,truth,col="red",lwd=2)




# Pòlia urn and Chinese Restaurant Process --------------------------------

chinese.rest <- function(dati, M, mu, s){
  pr= M/(M+length(dati))
  sim <- rep(0,5000)
  for(i in 1:5000){
    do <- rbinom(1,1,pr)
    if(do){
      sim[i] <- rnorm(1,mu,s)
    }else{
      sim[i] <- sample(dati, size=1, replace=T)
    }
  }
  thet <- rbeta(5000,shape1 = 1, shape2 = M+length(dati))
  prob <- rep(0,5000)
  prob[1] <- thet[1]
  for(i in 2:5000){
    prob[i]<- thet[i]*prod(1 - thet[1:i-1])
  }
  dat <- sample(sim, size= 5000, prob=prob,replace=T)
  return(dat)
}

mu <- mean(samples.2000); mu
se <- sd(samples.2000); se

risul <- chinese.rest(samples.2000, M = 1, mu = mu, s = se)

par(mfrow=c(1,2))

plot(ecdf(risul))
lines(x,truth.cdf,col="red",lwd=2)
lines(density(risul))

hist(risul, probability = T, breaks = 20)
lines(x,truth,col="red",lwd=2)
plot(ecdf(samples))
lines(x,truth.cdf,col="red",lwd=2)

