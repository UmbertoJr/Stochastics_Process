##############à############### Autocorrelation Function
data("AirPassengers")
plot(AirPassengers)
AP.dec <- decompose(AirPassengers, type = 'multiplicative')
plot(AP.dec)

ran <- AP.dec$random[7:138]
acf.ran <- acf(ran)

acf(ran, lag.max = 30, plot = T)

############# White Noise case
eps  <- rnorm(100)
plot(ts(eps))
acf(eps)


########### Airpassengers ACF
frequency(AirPassengers)
acf(AirPassengers, lag.max = 32)

#as numeric
a = as.numeric(AirPassengers)
acf(a, lag.max = 40)


# Partial correlation function

c(x <- rnorm(100), y <- rnorm(100), z <- rnorm(100))

r.xy <- cor(x,y)
r.xz <- cor(x,z)
r.yz <- cor(y,z)

# formula
r.xy.given.z <- (r.xy - r.xz*r.yz)/(sqrt((1 - r.xz^2)*(1 - r.yz^2)))
r.xy.given.z

#regression

lm.xz <- lm(x~z)
r <- residuals(lm.xz)
lm.yz <- lm(y~z)
s  <- residuals(lm.yz)
cor(r,s)
#check
r.xy.given.z == cor(r,s)

#above cases
pacf(ran)
pacf(eps)



# Autoregressive Model ----------------------------------------------------

par(mfrow=c(2,1), mar= c(1,3,1,1))
alp= 0.5
x <- arima.sim(model = list(ar=alp), n=200, sd=1)
plot(x)
x10 <- arima.sim(model = list(ar=alp), n=200, sd=10)
plot(x10)
par(mfrow=c(1,1), mar=c(4,4,1,1))

#second way
eps <- rnorm(200)
x <- eps
for(t in 2:200){
  x[t]<- 0.7 * x[t-1] + eps[t]
}
plot(x, type ='l')

### ACF and PACF
acf(x)
pacf(x)

acf(x10)
pacf(x10)

#real Case
Z <- read.table('pounds_nz.dat', header = T)
Z.ts <- ts(Z, start = 1991, frequency = 4)
plot(Z.ts)
plot(diff(Z.ts))
acf(diff(Z.ts))
pacf(diff(Z.ts))

########### AR(p) model
ar=c(0.9,-0.7,0.3)
abs(polyroot(ar))
x <- arima.sim(model = list(ar=ar), n= 200, sd=2)
plot(x)


ar = c(1,-1)
abs(polyroot(ar))
x <- arima.sim(model = list(ar=ar), n= 200, sd=2)
plot(x)


############ Tests for AR(1) model

eps <- rnorm(200)
x.rw <- eps
for(t in 2:200){
  x.rw[t] <- x.rw[t-1] + eps[t]
}
plot(x.rw, type = 'l')
  
x.st <- eps
for(t in 2:200){
  x.st[t] <- .7*x.st[t-1] + eps[t]
}
plot(x.st, type = 'l')

library(tseries)
adf.test(x.rw)
adf.test(x.st)

pp.test(x.rw)
pp.test(x.st)


#real case
Z <- read.table('pounds_nz.dat', header = T)
Z.ts <- ts(Z, start = 1991, frequency = 4)
plot(Z.ts)
adf.test(Z.ts)
pp.test(Z.ts)



# Autocorr and Partial Corr for  AR model ---------------------------------

acf.ar <- ARMAacf(ar=.7, lag.max = 10)
plot(seq_along(acf.ar) - 1, acf.ar, type = 'h')

pacf.ar <- ARMAacf(ar=.7, lag.max = 10, pacf = T)
plot(seq_along(pacf.ar) - 1, pacf.ar, type = 'h')




# MA model ----------------------------------------------------------------
ma = c(1,0.4,-.1)
x <- arima.sim(model = list(ma=ma), n=200, sd=1)
plot(x)
acf(x)
pacf(x)

m <- arima(x, order = c(0,0,5))
m
tsdiag(m)



######## Exercise on Real Data
CBE  <- read.table('CBE.dat', header = T)
Beer.ts <- ts(CBE[,2], start = 1958, frequency = 12)
plot(Beer.ts)

beer.dec <- decompose(Beer.ts)
Y <- ts(as.vector(beer.dec$random[7:390]))
plot(Y)
acf(Y)
pacf(Y)
plot(log(Beer.ts))
m = arima(Beer.ts, order = c(3,2,2))
m
tsdiag(m)


######### WORKED EXAMPLE
library(MASS)
plot(deaths)
acf(deaths)
pacf(deaths)
# the plot and the ACF suggest periodicity so...
y <- diff(deaths,lag = 12)
plot(y)
acf(y)
pacf(y)

ar(y)

m <- arima(deaths, order = c(2,0,0), seasonal = list(order = c(0,1,0)))
m
tsdiag(m)


m2 <- arima(deaths, order = c(2,0,0), seasonal = list(order = c(1,0,0)))
m2
tsdiag(m2)


m3 <- arima(deaths, order = c(2,0,0), seasonal = list(order = c(1,1,0)))
m3
tsdiag(m3)

pm3 <- predict(m3, n.ahead = 12, se.fit = T)
ts.plot(cbind(deaths, pm3$pred))



######### Some Other data sets

plot(sunspots)
acf(sunspots)
pacf(sunspots)

y <- diff(sunspots, lag = 12,differences = 3)
plot(y)
acf(y)
pacf(y)

m <- arima(sunspot.year, order = c(3,3,5), seasonal = list(order = c(1,0,0)))
m

pm3 <- predict(m, n.ahead = 12, se.fit = T)
ts.plot(cbind(sunspot.year, pm3$pred))


#### Regression Model with Autocorrelated Errors
Global <- scan('global.dat')
Global.ts <- ts(Global, start=c(1856,1), end = c(2005,12), frequency = 12)
plot(Global.ts)
Global.annual <- aggregate(Global.ts, FUN = mean)
plot(Global.annual)

temps.1970 <- window(Global.ts, start = 1970)
plot(temps.1970)
times.1970 <- time(temps.1970)

lm.1970 <- lm(temps.1970 ~ times.1970)
abline(lm.1970)

r.lm.1970 <- residuals(lm.1970)
plot(r.lm.1970, type = 'l')
acf(r.lm.1970)
pacf(r.lm.1970)



# Generalized Least Squares -----------------------------------------------
library(nlme)
# It's possible to improve the simple regression by studying the error behaviour
lm.1970 <- gls(temps.1970 ~ times.1970)
lm.1970

gls.1970 <- gls(temps.1970 ~ times.1970, correlation = corAR1(0.7,fixed = T))
gls.1970
summary(gls.1970)


gls.1970.2 <- gls(temps.1970 ~ times.1970, correlation = corAR1(0.7))
gls.1970.2
summary(gls.1970.2)


###### Predictions
new.times <- seq(from=2006, to= 2012, by=1/12)
new.temps <- predict(gls.1970, newdata = data.frame(times.1970=new.times))
new.temps <- ts(new.temps, start=2006, frequency = 12)
ts.plot(cbind(temps.1970, new.temps), sub="From 1970")


# Real Case Philliphs-Ouliars cointegration test -------------------------------------

xrates <- read.table('US_rates.dat', header = T)
cor(xrates$UK, xrates$EU)
po.test(cbind(xrates$UK, xrates$EU))
# high probability of cointegration









