
# AirPassengers data
data("AirPassengers")
AirPassengers
class(AirPassengers)
summary(AirPassengers)
a = aggregate(AirPassengers)
plot(AirPassengers)
cycle(AirPassengers)
boxplot(AirPassengers ~ cycle(AirPassengers) )


#Maine data
Main.month <- read.table('maine.dat', header = T)
Main.month.ts <- ts(Main.month$unemploy, start = c(1996,1), frequency = 12)
plot(Main.month.ts, ylab= "Unemployed %")



#CBE data
CBE <- read.table('CBE.dat', header = T)
Elect.ts <- ts(CBE[,3], start = 1958, frequency = 12)
Beer.ts <- ts(CBE[,2], start = 1958, frequency = 12)
Chocr.ts <- ts(CBE[,1], start = 1958, frequency = 12)
plot(cbind(Elect.ts, Beer.ts, Chocr.ts), main= "Suitable title")


# Global temperature data
Global  <- scan('global.dat')
Global.ts = ts(Global, start = c(1856,1), end = c(2005,12), frequency = 12)
Global.annual <- aggregate(Global.ts, FUN = mean)
par(mfrow=c(2,1), mar= c(1,1,1,1))
plot(Global.ts)
plot(Global.annual)
par(mfrow=c(1,1))



# Additive Model ----------------------------------------------------------

Elec.decom.add <-  decompose(Elect.ts)
names(Elec.decom.add)
plot(Elec.decom.add)
ts.plot(cbind(Elec.decom.add$trend, Elec.decom.add$trend + Elec.decom.add$seasonal))


# Multiplicative Modelling ------------------------------------------------

ELect.decom.mult <- decompose(Elect.ts, type = 'multiplicative')
names(ELect.decom.mult)
plot(ELect.decom.mult)
mean(ELect.decom.mult$random, na.rm = TRUE)
var(ELect.decom.mult$random, na.rm = T)



# stl method --------------------------------------------------------------
?stl

Elec.stl <- stl(Elect.ts, s.window = 'periodic')
names(Elec.stl)
plot(Elec.stl)




# HoltWinters -------------------------------------------------------------
X <- c(3,-2,4,-1)
x.ts <- ts(X, start = 1, frequency = 12)
#exponential Smoothing
?HoltWinters
ES <- HoltWinters(x.ts, alpha = 0.4, beta = F, gamma = F)
ES
names(ES)
fitted(ES)
coefficients(ES)
predict(ES, n.ahead = 4, prediction.interval = T)


# real case axample -------------------------------------------------------
Motor.dat <- read.table('Motororg.dat', header = T)
Comp.ts <- ts(Motor.dat$complaints, start = c(1996,1), frequency = 12)
#par(mar=c(4,4,2,2))
plot(Comp.ts)

Comp.hw1 <- HoltWinters(Comp.ts, alpha = NULL, beta = F, gamma = F)
Comp.hw1
Comp.hw1$SSE
plot(Comp.hw1)
Comp.pred <- predict(Comp.hw1, n.ahead = 12, prediction.interval = T)
ts.plot(cbind(Comp.ts, Comp.pred))



# Extimating the level and Trend of a Time series locally -----------------
X <- c(3,-2,4,-1)
x.ts <- ts(X, start = 1, frequency = 12)
ES <- HoltWinters(x.ts, alpha = 0.4, beta = 0.3, gamma = F)
ES
names(ES)
fitted(ES)
coefficients(ES)
predict(ES, n.ahead = 4, prediction.interval = T)

# real data Comp
Comp.hw2 <- HoltWinters(Comp.ts, alpha = NULL, beta = NULL, gamma = F)
Comp.hw2$SSE
plot(Comp.hw2)

# Extimating the level, the Trend and the Seasonality of a Time series locally -----------------
x <- c(3,8,6,10,22)
p <- 2
xts <- ts(x, start = c(1,1),frequency = p)
HW <- HoltWinters(xts, alpha = 0.3, beta = 0.4, gamma = 0.5,
                  l.start = x[2], b.start = x[2]-x[1],
                  s.start = c(0.75,0),seasonal = 'additive')
HW
plot(HW)


# Holt-Winters  Method with multiplicative seasonality --------------------
# Real case 
data("AirPassengers")
#### additive
plot(AirPassengers)
AP.add <- HoltWinters(AirPassengers)
AP.add
plot(AP.add)

### Multiplicative
AP.mult <- HoltWinters(AirPassengers, seasonal = 'multiplicative')
AP.mult
plot(AP.mult)
c(AP.add$SSE, AP.mult$SSE)



