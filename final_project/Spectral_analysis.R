n <- 6
x <- rnorm(n)

#explanatory variables
t <- 1:n
COS1 <- cos(2 * pi * t/n)
SIN1 <- sin(2 * pi * t/n)
COS2 <- cos(4 * pi * t/n)
SIN2 <- sin(4 * pi * t/n)
COS3 <- cos(6 * pi * t/n)

# perform the linear regression

m <- lm(x ~ COS1 + SIN1 + COS2 + SIN2 + COS3)
summary(m)

mean(x)

all.coef <- coefficients(m)
a0 <- as.numeric(all.coef[1])
a1 <- as.numeric(all.coef[2])
b1 <- as.numeric(all.coef[3])
a2 <- as.numeric(all.coef[4])
b2 <- as.numeric(all.coef[5])
a3 <- as.numeric(all.coef[6])

c(a0, a1, b1, a2, b2, a3)


# Partition of the variance
sum((x - mean(x))^2)/n
var(x)*(n-1)/n

R1.sq <- a1^2 + b1^2
R2.sq <- a2^2 + b2^2
(R1.sq + R2.sq)/2 + a3^2


freq.x <- (1:(n/2))/n
spectrum.x <- c(R1.sq/2,R2.sq/2,a3^2)
plot(freq.x, spectrum.x)

spectrum(x, log='no')

s <- spectrum(x, log='no', plot=F)
plot(s$freq, 2* s$spec/n, type='h')



# Example Frequency Partition ---------------------------------------------
n <- 2048
x <- rnorm(n)
spectrum(x, log = 'no')
spectrum(x, span= 100,log = 'no')






# REAL CASE ANALYSIS ------------------------------------------------------

SOI <- read.table('soi.dat', header = T)$SOI
length(SOI)
soi.ts <- ts(SOI, start= c(1866,1), frequency = 12)
plot(soi.ts)
soi.spec <- spectrum(SOI, span= sqrt(2*length(SOI)))
plot(soi.spec)
which.max(soi.spec$spec)
1/soi.spec$freq[39]

detach(soi.data)
