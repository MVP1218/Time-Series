# R code need to install packages TSA, fUnitRoots
library(TSA)
library(fUnitRoots)

data(ma1.1.s)
acf(ma1.1.s)
acf(ma1.1.s, xaxp=c(0,20,10))
acf(ma1.1.s, ci.type="ma")


data(ar1.s); acf(ar1.s); pacf(ar1.s)

data(ar2.s); acf(ar2.s); pacf(ar2.s)
polyroot(c(1, -1.5, 0.75))

data(arma11.s); acf(arma11.s); pacf(arma11.s)

data(oil.price)
acf(as.vector(oil.price))
acf(diff(as.vector(log(oil.price))))

data(rwalk)
acf(diff(rwalk,difference=2),ci.type='ma')
acf(diff(rwalk))

m1=ar(diff(rwalk))
m1$order

library(fUnitRoots)
adfTest(rwalk,lags=8,type=c("c")) #option: nc, c, ct
adfTest(rwalk,lags=0,type=c("c"))
adfTest(rwalk,lags=8,type=c("ct"))
adfTest(rwalk,lags=0,type=c("ct"))


set.seed(92397)
test=arima.sim(model=list(ar=c(rep(0,11),.8),
ma=c(rep(0,11),0.7)),n=120)
res=armasubsets(y=test,nar=14,nma=14)
plot(res)

data(larain); 
qqnorm(log(larain)); qqline(log(larain))
acf(log(larain))

data(hare); 
m2=BoxCox.ar(hare)
acf(hare^.5)
pacf(hare^.5)

res=armasubsets(y=diff(log(oil.price)),nar=7,nma=7)
plot(res)
acf(as.vector(diff(log(oil.price))))
pacf(as.vector(diff(log(oil.price))))



### 

data(hare); 
qqnorm(hare)
qqline(hare)
m2=BoxCox.ar(hare)
acf(hare^.5)
pacf(hare^.5)



a0=arima.sim(n=200, model=list(order=c(0,0,0)))
a1=arima.sim(n=200, model=list(order=c(0,1,0)))
a2=arima.sim(n=200, model=list(order=c(1,0,0), ar=0.9))
a22=arima.sim(n=200, model=list(order=c(1,0,1), ar=0.9, ma=0.8))

plot; acf

a3=diff(a1)
#a4=diff(a1, differences = 2)

m=ar(a3)
library(fUnitRoots)
adfTest(a3, lag=m$order)
adfTest(a3, lag=m$order, type="c")

acf(a3, ci.type="ma")
pacf(a3)

arima(a3, order=c())
arima(a3, order=c(), include.mean=0)
arima(arma11.s, order=c(1,0,1),method='CSS')  #ML

armasubsets(a22, nar=4, nma=4)


data(color)
m1=arima(color, order=)
plot(rstandard(m1))
abline(h=0)
qqnorm(residuals(m1))
qqline(residuals(m1))
acf(residuals(m1))
acf(residuals(arima(color, order=c(2,0,0))))
acf(residuals(m1),plot=F)$acf
signif(acf(residuals(m1),plot=F)$acf[1:6],2)
tsdiag(m1,gof=15,omit.initial=F)


data(hare)
m1=arima(sqrt(hare), order=c(3,0,0))
m2=arima(sqrt(hare), order=c(3,0,0), fixed=c(NA, 0, NA, NA))

