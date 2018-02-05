

# simple commands for sampling from a
# Dirichlet distribution and a 
# Dirichlet Process

require(Compositional)
require(MCMCpack)

#Sampling from a bivariate Dirichlet 


draws1 <- rdirichlet(200, c(1,1,1) ) #Uniform Density
draws2 <- rdirichlet(200, c(.1,.1,.1) ) #Concentrated on the vertexes
draws3 <- rdirichlet(200, c(3,3,3) ) # Unimodal 
draws4 <- rdirichlet(200, c(10,1,1) ) #Skewed

par(mfrow=c(2,2))

bivt.contour(draws1)
title("200 random draws for a D(1,1,1)")
bivt.contour(draws2)
title("200 random draws for a D(.1,.1,.1)")
bivt.contour(draws3)
title("200 random draws for a D(3,3,3)")
bivt.contour(draws4)
title("200 random draws for a D(10,1,1)")
#
#Same picture with more points  
#

draws21 <- rdirichlet(8000, c(1,1,1) ) #Uniform Density
draws22 <- rdirichlet(800, c(.1,.1,.1) ) #Concentrated on the vertexes
draws23 <- rdirichlet(800, c(3,3,3) ) # Unimodal 
draws24 <- rdirichlet(800, c(10,1,1) ) #Skewed

bivt.contour(draws21)
title("800 draws for a D(1,1,1)")
bivt.contour(draws22)
title("800 draws for a D(.1,.1,.1)")
bivt.contour(draws23)
title("800 draws for a D(3,3,3)")
bivt.contour(draws24)
title("800 random draws for a D(10,1,1)")


#Come fare una densità bivariata Dirichlet
x<-seq(0.01,.99, length=100)
y<-x
fd=function(d1,d2, astar) {
  xx=cbind(d1, d2, 1-d1-d2)
  return(ddirichlet(xx, alpha=astar))
}

fd(.1,.1, c(1,2,2)) 
z <- outer(x, y, "fd", astar=c(1,1,1))
z1<-outer(x, y, "fd", astar=c(2,15,2))
z2 <- outer(x, y, "fd", astar=c(3,3,3))
z3<-outer(x, y, "fd", astar=c(10,1,1))

par(mfrow=c(1,1))
z[is.na(z)] <- 0
z1[is.na(z1)] <- 0
z2[is.na(2)] <- 0
z3[is.na(z3)] <- 0
op <- par(bg = "grey")
persp(x, y, z, theta = 50, phi = 20, expand =0.5, 
      zlab="", ylab="x2 ", xlab="x1", col = "red",
      main="Density of a D(1,1,1)")
persp(x, y, z1, theta = 50, phi = 20, expand =0.5, 
      zlab="", ylab="x2 ", xlab="x1", col = "red",
      main="Density of a D(2,15,2)")
persp(x, y, z2, theta = 50, phi = 20, expand =0.5, 
      zlab="", ylab="x2 ", xlab="x1", col = "red",
      main="Density of a D(3,3,3)")
persp(x, y, z3, theta = 50, phi = 20, expand =0.5, 
      zlab="", ylab="x2 ", xlab="x1", col = "red",
      main="Density of a D(10,1,1)")



#
# Sampling from a Dirichlet Process (M, 
#g()=Gaussian(0,1))
#

sample.dir<-function(nn=10, M=5){
x <- seq(-4,4, length=11)
y<-c()
y[1]<-pnorm(x[1])
for(i in 2:11) y[i]<-pnorm(x[i])-pnorm(x[(i-1)])
y<-c(y, 1-pnorm(x[11]))
param<-M*y
#return(param)
sample.dir<-rdirichlet(nn,param)
draw<-apply(t(sample.dir), 2, cumsum)
return((draw))}

draws<-sample.dir()
xx<-c(seq(-4,4, length=11),5)
matplot(xx, draws, col=1:10, type="b")
curve(pnorm(x), add=T)

draws1<-sample.dir(10, M=.1)
matplot(xx, draws1, col=1:10, type="b")
curve(pnorm(x), add=T)



draws1<-sample.dir(10, M=15)
matplot(xx, draws1, col=1:10, type="b")
curve(pnorm(x), add=T)

draws1<-sample.dir(10, M=50)
matplot(xx, draws1, col=1:10, type="b")
curve(pnorm(x), add=T)
