s.20 <- rnorm(20)
s.200 <- rnorm(200)
s.2000 <- rnorm(2000)

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

# simulation

sim.20 <- matrix(nrow = 100, ncol = 5000)
sim.200 <- matrix(nrow = 100, ncol = 5000)
sim.2000 <- matrix(nrow = 100, ncol = 5000)
M=300
for(s in 1:100){
  sim.20[s,]<- chinese.rest(s.20,M= M,0,1)
  sim.200[s,]<- chinese.rest(s.200,M= M,0,1)
  sim.2000[s,]<- chinese.rest(s.2000,M= M,0,1)
}
save(sim.20,sim.200,sim.2000,file = 'simulation_normal_m300.RData')

cases <- list(sim.20,sim.200,sim.2000)
casi <- c('20 obs','200 obs','2000 obs')
par(mfrow=c(3,1), mar=c(2,2,1,1))
for(m in 1:3){
  curve(pnorm,  col='black', lwd=2, from = -5,to=5,main = casi[m])
  for(i in 1:100){
    plot(ecdf(cases[[m]][i,]),verticals=TRUE, do.points=FALSE, lwd = 0.00001, lty=1
         , add=T,col=randomColor())
  }
  curve(pnorm,  col='black', lwd=2, from = -5,to=5, add=T)
}

par(mfrow=c(1,1))
for(m in 1:3){
  curve(dnorm,  col='black', lwd=2, from = -5,to=5,main = casi[m],ylim=c(0,.5))
  for(i in 1:100){
    lines(density(cases[[m]][i,]), lwd = 0.00001, lty=1
          ,col=randomColor())
  }
  curve(dnorm,  col='black', lwd=2, from = -5,to=5, add=T)
}

