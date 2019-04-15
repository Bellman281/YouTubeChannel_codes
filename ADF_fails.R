library(urca)
library(forecast)
n=50
e_t<- rnorm(n)
#initial
x_i=0
t=1
X_t<-vector()
X_t[1]=x_i
X_t[2]=1
X_t[3]=3


teta1=0.6
teta2=0.3

for (t in 1:n)
{X_t[t+2]=teta1*X_t[t]+teta2*X_t[t+1]+e_t[t]}


p<-summary(ur.df(X_t,type="none",selectlags = "AIC"))
#summary(ur.df(X_t,type="none",selectlags = "AIC"))

#summary(ur.df(X_t,type="none",lags = 2))


p@teststat[1]
if (p@teststat[1] < p@cval[1]) {print("stationary")}
if (p@teststat[1] >= p@cval[1]) {print("non - stationary")}
if (p@teststat[1] >= p@cval[2]) {print("non - stationary")}
if (p@teststat[1] >= p@cval[3]) {print("non - stationary")}

auto.arima(X_t)
plot(X_t)

plot(X_t[1:200])



t=5

dir_t<-sign(X_t[2:n]-X_t[1:(n-1)])


p2<-summary(ur.df(dir_t,type="none",selectlags = "AIC"))
summary(ur.df(dir_t,type="none",selectlags = "AIC"))



p2@teststat[1]
if (p2@teststat[1] < p2@cval[1]) {print("stationary")}
if (p2@teststat[1] >= p2@cval[1]) {print("non - stationary")}
if (p2@teststat[1] >= p2@cval[2]) {print("non - stationary")}
if (p2@teststat[1] >= p2@cval[3]) {print("non - stationary")}


auto.arima(dir_t)
plot(X_t)
lines(dir_t)
plot(X_t[1:200])
lines(dir_t[1:200])







# 
# 
teta=0.51
# 
 


 
 
 l=(teta+sqrt(teta*teta+4*teta))/2 ;l
 l=(teta-sqrt(teta*teta+4*teta))/2 ;l
 